package com.jdc.recipe_service.dev.facade;

import com.jdc.recipe_service.dev.facade.DevYoutubeRecipeExtractionFacade.JobCreateResult;
import com.jdc.recipe_service.dev.service.image.DevImageGenRouterService;
import com.jdc.recipe_service.dev.service.quota.DevYoutubeQuotaService;
import com.jdc.recipe_service.dev.service.recipe.ingredient.DevRecipeIngredientPersistService;
import com.jdc.recipe_service.dev.service.youtube.YoutubeSignalDetector;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeInfo;
import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.meta.RecipeYoutubeExtractionInfoRepository;
import com.jdc.recipe_service.domain.repository.meta.RecipeYoutubeInfoRepository;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.media.EvidenceLevel;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import com.jdc.recipe_service.service.media.YtDlpService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionTemplate;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * DevYoutubeRecipeExtractionFacade 단위 테스트.
 *
 * Phase 1 (createJob) 검증:
 *  1. 사전 검증 순서: URL pattern → videoId 형식 → imageGenModel 화이트리스트 (모두 quota 차감 전)
 *  2. idempotency reused → created=false, 쿼터/저장 모두 호출 안 함
 *  3. 신규 키 → 쿼터 차감 + job 저장 (imageGenerationModel + tokenCost=2 + usedGeminiAnalysis=false + quotaUsedOn)
 *  4. namespacing: dev-yt:{userId}:{sha256(rawKey)} 형식
 *
 * Phase 2 (async) 검증:
 *  - dedup 분기 (legacy job vs dev V3 job) — refund overload 선택 정확성
 *  - durable idempotency — managed entity가 source of truth, outer가 stale이어도 중복 환불 차단
 *  - tx 실패 격리 — refund tx 실패 시 outer 원본 유지 → catch 재시도 가능
 *  - Gemini upgrade atomicity — chargeGeminiUpgrade DAO + managed save가 단일 tx, 실패 시 +3 누수 방지
 *  - cross-midnight 정확성 — quotaUsedOn 기준 date-aware charge/refund 호출
 *
 * 주의: async 테스트들은 transactionTemplate stub + 다중 findById 인스턴스로 tx 경계를 시뮬레이션하므로,
 * 실제 DB rollback 시맨틱이 아니라 "managed가 source of truth"라는 invariant를 고정한다.
 * 진짜 tx 원자성은 별도 integration 테스트 (real DataSource)로 검증해야 한다.
 */
@ExtendWith(MockitoExtension.class)
class DevYoutubeRecipeExtractionFacadeTest {

    @Mock YtDlpService ytDlpService;
    @Mock GrokClientService grokClientService;
    @Mock GeminiMultimodalService geminiMultimodalService;
    @Mock RecipeService recipeService;
    @Mock AsyncImageService asyncImageService;
    @Mock DevYoutubeQuotaService devYoutubeQuotaService;
    @Mock DevImageGenRouterService devImageGenRouterService;
    @Mock YoutubeSignalDetector signalDetector;
    @Mock RecipeRepository recipeRepository;
    @Mock RecipeGenerationJobRepository jobRepository;
    @Mock RecipeYoutubeInfoRepository recipeYoutubeInfoRepository;
    @Mock RecipeYoutubeExtractionInfoRepository extractionInfoRepository;
    @Mock DevRecipeIngredientPersistService devIngredientPersist;
    @Mock TransactionTemplate transactionTemplate;

    @InjectMocks DevYoutubeRecipeExtractionFacade facade;

    private static final Long USER_ID = 1L;
    private static final String CLIENT_KEY = "key-abc";
    private static final String VALID_URL = "https://www.youtube.com/watch?v=dQw4w9WgXcQ";
    private static final String VALID_MODEL = "gemini-2.5-flash-image";

    // ---------- 사전 검증 (URL/videoId/model) — 모두 quota 차감 전에 거부 ----------

    @Test
    @DisplayName("URL pattern 미일치 → INVALID_URL_FORMAT, quota/job 호출 없음")
    void invalidUrlPattern_throws() {
        assertThatThrownBy(() -> facade.createYoutubeExtractionJob(
                "https://example.com/watch?v=dQw4w9WgXcQ", VALID_MODEL, USER_ID, CLIENT_KEY))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_URL_FORMAT);

        verify(devYoutubeQuotaService, never()).tryStartOrThrow(any());
        verify(jobRepository, never()).saveAndFlush(any());
    }

    @Test
    @DisplayName("YouTube channel 페이지 (videoId 추출 불가) → INVALID_URL_FORMAT, quota 차감 없음")
    void channelUrlNoVideoId_throwsBeforeQuota() {
        assertThatThrownBy(() -> facade.createYoutubeExtractionJob(
                "https://www.youtube.com/@somechannel", VALID_MODEL, USER_ID, CLIENT_KEY))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_URL_FORMAT);

        verify(devYoutubeQuotaService, never()).tryStartOrThrow(any());
        verify(jobRepository, never()).saveAndFlush(any());
    }

    @Test
    @DisplayName("watch?v= 빈 값도 INVALID_URL_FORMAT (videoId 11자 검증)")
    void emptyVideoIdParam_throws() {
        assertThatThrownBy(() -> facade.createYoutubeExtractionJob(
                "https://www.youtube.com/watch?v=", VALID_MODEL, USER_ID, CLIENT_KEY))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_URL_FORMAT);

        verify(devYoutubeQuotaService, never()).tryStartOrThrow(any());
    }

    @Test
    @DisplayName("URL은 valid이지만 imageGenModel이 화이트리스트 위반 → UNSUPPORTED_IMAGE_MODEL, quota 차감 없음")
    void invalidImageModel_throwsBeforeQuota() {
        assertThatThrownBy(() -> facade.createYoutubeExtractionJob(
                VALID_URL, "dall-e-3", USER_ID, CLIENT_KEY))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNSUPPORTED_IMAGE_MODEL);

        verify(devYoutubeQuotaService, never()).tryStartOrThrow(any());
        verify(jobRepository, never()).saveAndFlush(any());
    }

    // ---------- idempotency reused ----------

    @Test
    @DisplayName("동일 idempotency 재요청 → created=false, 쿼터/저장 호출 안 함")
    void reusedKey_returnsCreatedFalse() {
        RecipeGenerationJob existing = RecipeGenerationJob.builder().userId(USER_ID).build();
        ReflectionTestUtils.setField(existing, "id", 99L);
        given(jobRepository.findByIdempotencyKey(any())).willReturn(Optional.of(existing));

        JobCreateResult result = facade.createYoutubeExtractionJob(
                VALID_URL, VALID_MODEL, USER_ID, CLIENT_KEY);

        assertThat(result.created()).isFalse();
        assertThat(result.jobId()).isEqualTo(99L);
        verify(devYoutubeQuotaService, never()).tryStartOrThrow(any());
        verify(jobRepository, never()).saveAndFlush(any());
    }

    // ---------- 신규 키: 쿼터 차감 + job 저장 ----------

    @Test
    @DisplayName("신규 키 + 모든 검증 통과: quota 차감 + job 저장 (imageGenerationModel/tokenCost=2/usedGemini=false/quotaUsedOn=차감일)")
    void newKey_chargesQuotaAndSavesJob() {
        LocalDate consumedOn = LocalDate.of(2026, 4, 26);
        given(devYoutubeQuotaService.tryStartOrThrow(USER_ID)).willReturn(consumedOn);
        given(jobRepository.findByIdempotencyKey(any())).willReturn(Optional.empty());
        given(jobRepository.saveAndFlush(any(RecipeGenerationJob.class))).willAnswer(inv -> {
            RecipeGenerationJob j = inv.getArgument(0);
            ReflectionTestUtils.setField(j, "id", 200L);
            return j;
        });

        JobCreateResult result = facade.createYoutubeExtractionJob(
                VALID_URL, "gpt-image-2-medium", USER_ID, CLIENT_KEY);

        assertThat(result.created()).isTrue();
        assertThat(result.jobId()).isEqualTo(200L);

        verify(devYoutubeQuotaService).tryStartOrThrow(USER_ID);

        ArgumentCaptor<RecipeGenerationJob> captor = ArgumentCaptor.forClass(RecipeGenerationJob.class);
        verify(jobRepository).saveAndFlush(captor.capture());
        RecipeGenerationJob saved = captor.getValue();
        assertThat(saved.getImageGenerationModel()).isEqualTo("gpt-image-2-medium");
        assertThat(saved.getTokenCost()).isEqualTo(DevYoutubeQuotaService.COST_BASIC);
        assertThat(saved.getUsedGeminiAnalysis()).isFalse();
        // cross-midnight 환불 정확성: 차감 시점 날짜를 job에 저장
        assertThat(saved.getQuotaUsedOn()).isEqualTo(consumedOn);
    }

    @Test
    @DisplayName("저장되는 idempotencyKey는 'dev-yt:{userId}:{sha256(rawKey)}' 형식")
    void newKey_idempotencyKeyIsNamespaced() {
        given(jobRepository.findByIdempotencyKey(any())).willReturn(Optional.empty());
        given(jobRepository.saveAndFlush(any(RecipeGenerationJob.class))).willAnswer(inv -> inv.getArgument(0));

        facade.createYoutubeExtractionJob(VALID_URL, VALID_MODEL, USER_ID, CLIENT_KEY);

        ArgumentCaptor<RecipeGenerationJob> captor = ArgumentCaptor.forClass(RecipeGenerationJob.class);
        verify(jobRepository).saveAndFlush(captor.capture());
        String stored = captor.getValue().getIdempotencyKey();

        // dev-yt: prefix + userId + sha256-hex(64자)
        assertThat(stored).startsWith("dev-yt:" + USER_ID + ":");
        String hashPart = stored.substring(("dev-yt:" + USER_ID + ":").length());
        assertThat(hashPart).hasSize(64);
        assertThat(hashPart).matches("[0-9a-f]+"); // hex만
    }

    // =========================================================================
    // Phase 2 (async) — focused tests on dedup refund + idempotency
    // =========================================================================

    private static final String VIDEO_ID = "dQw4w9WgXcQ";

    /**
     * transactionTemplate의 callback을 즉시 invoke하도록 stub.
     * Spring의 modern signature: executeWithoutResult(Consumer&lt;TransactionStatus&gt;).
     * (이전 TransactionCallbackWithoutResult로 cast하면 ClassCastException 발생.)
     */
    @SuppressWarnings("unchecked")
    private void stubTransactionTemplate() {
        doAnswer(inv -> {
            Consumer<TransactionStatus> cb = inv.getArgument(0);
            cb.accept(null);
            return null;
        }).when(transactionTemplate).executeWithoutResult(any());
    }

    @Test
    @DisplayName("이미지 생성 실패: recipe는 FAILED/default image로 저장한 뒤 job COMPLETED")
    @SuppressWarnings("unchecked")
    void async_imageGenerationFails_savesFailedImageAndCompletesJob() {
        Long jobId = 120L;
        Long recipeId = 700L;
        LocalDate consumedOn = LocalDate.of(2026, 4, 30);
        RecipeGenerationJob job = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC, consumedOn);
        given(jobRepository.findById(jobId)).willReturn(Optional.of(job));
        stubTransactionTemplate();
        given(transactionTemplate.execute(any())).willAnswer(inv -> {
            TransactionCallback<Long> cb = inv.getArgument(0);
            return cb.doInTransaction(null);
        });

        given(recipeYoutubeInfoRepository.findByVideoId(VIDEO_ID)).willReturn(Optional.empty());
        given(recipeRepository.findFirstOfficialByYoutubeUrl(any(), any())).willReturn(Optional.empty());

        YtDlpService.YoutubeFullDataDto data = new YtDlpService.YoutubeFullDataDto(
                VIDEO_ID, "", "title", "description with ingredients",
                "", "scriptTimecoded", "scriptPlain",
                "channel", "channelId", "thumbnail", "profile", 100L, 200L, 300L);
        given(ytDlpService.getVideoDataFull(VALID_URL)).willReturn(data);

        YoutubeSignalDetector.SignalReport signals =
                new YoutubeSignalDetector.SignalReport(true, true, false);
        given(signalDetector.detectSignals(any(), any(), any())).willReturn(signals);
        given(signalDetector.computeEvidence(signals, false)).willReturn(EvidenceLevel.HIGH);

        RecipeCreateRequestDto dto = new RecipeCreateRequestDto();
        dto.setIsRecipe(true);
        dto.setTitle("youtube recipe");
        dto.setIngredients(List.of(
                ing("감자", "1", "개"),
                ing("양파", "1", "개"),
                ing("소금", "1", "작은술")));
        given(grokClientService.generateRecipeStep1(any(), any()))
                .willReturn(CompletableFuture.completedFuture(dto));

        given(asyncImageService.buildPromptFromDto(any())).willReturn("image prompt");
        given(devImageGenRouterService.generate(any(), any(), any(), any())).willReturn(List.of());

        PresignedUrlResponse response = PresignedUrlResponse.builder()
                .recipeId(recipeId)
                .uploads(List.of())
                .build();
        given(recipeService.createRecipeAndGenerateUrls(
                any(), eq(90121L), eq(RecipeSourceType.YOUTUBE), any()))
                .willReturn(response);
        Recipe recipe = mock(Recipe.class);
        given(recipeRepository.findById(recipeId)).willReturn(Optional.of(recipe));

        facade.processYoutubeExtractionAsync(jobId, VALID_URL, VALID_MODEL, USER_ID);

        ArgumentCaptor<com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest> requestCaptor =
                ArgumentCaptor.forClass(com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest.class);
        verify(recipeService).createRecipeAndGenerateUrls(
                requestCaptor.capture(), eq(90121L), eq(RecipeSourceType.YOUTUBE), any());
        RecipeCreateRequestDto savedDto = requestCaptor.getValue().getRecipe();
        assertThat(savedDto.getImageStatus()).isEqualTo(RecipeImageStatus.FAILED);
        assertThat(savedDto.getImageKey()).isEqualTo(AsyncImageService.DEFAULT_IMAGE_KEY);
        assertThat(job.getStatus()).isEqualTo(com.jdc.recipe_service.domain.type.JobStatus.COMPLETED);
        assertThat(job.getResultRecipeId()).isEqualTo(recipeId);
    }

    private RecipeGenerationJob mockJobWithCost(Long jobId, int tokenCost) {
        return mockJobWithCost(jobId, tokenCost, null);
    }

    private RecipeGenerationJob mockJobWithCost(Long jobId, int tokenCost, LocalDate quotaUsedOn) {
        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .userId(USER_ID)
                .tokenCost(tokenCost)
                .quotaUsedOn(quotaUsedOn)
                .build();
        ReflectionTestUtils.setField(job, "id", jobId);
        return job;
    }

    @Test
    @DisplayName("async dedup (legacy job, quotaUsedOn=null): video_id 매치 시 deprecated refund + completeJob, yt-dlp/추출 호출 없음")
    void async_dedupByVideoId_legacyJob_usesLegacyRefund() {
        Long jobId = 100L;
        // legacy job: quotaUsedOn=null (V20260426_006 마이그레이션 이전 row 시나리오)
        RecipeGenerationJob job = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC);
        given(jobRepository.findById(jobId)).willReturn(Optional.of(job));
        stubTransactionTemplate();

        Recipe existingRecipe = mock(Recipe.class);
        given(existingRecipe.getId()).willReturn(7L);
        RecipeYoutubeInfo existingInfo = mock(RecipeYoutubeInfo.class);
        given(existingInfo.getRecipe()).willReturn(existingRecipe);
        given(recipeYoutubeInfoRepository.findByVideoId(VIDEO_ID)).willReturn(Optional.of(existingInfo));

        facade.processYoutubeExtractionAsync(jobId, VALID_URL, VALID_MODEL, USER_ID);

        // legacy 분기: quotaUsedOn=null → deprecated overload (today 환불, V2 호환)
        verify(devYoutubeQuotaService).refund(USER_ID, DevYoutubeQuotaService.COST_BASIC);
        // date-aware overload는 호출 안 됨
        verify(devYoutubeQuotaService, never()).refund(any(), anyInt(), any(LocalDate.class));
        // 추출 자체는 호출 안 됨
        verify(ytDlpService, never()).getVideoDataFull(any());
        verify(grokClientService, never()).generateRecipeStep1(any(), any());
        verify(geminiMultimodalService, never()).generateRecipeFromYoutubeUrl(any(), any(), any());
        // refundAndNullifyCost가 tokenCost를 0으로 만들어 nullify save
        assertThat(job.getTokenCost()).isZero();
    }

    @Test
    @DisplayName("async dedup (dev V3 job, quotaUsedOn 채워짐): cross-midnight 정확성 — 차감일 기준 date-aware refund 호출")
    void async_dedupByVideoId_devV3Job_usesDateAwareRefund() {
        Long jobId = 110L;
        LocalDate consumedOn = LocalDate.of(2026, 4, 25); // 어제 차감했다고 가정 (자정 넘겨 dedup 발생)
        RecipeGenerationJob job = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC, consumedOn);
        given(jobRepository.findById(jobId)).willReturn(Optional.of(job));
        stubTransactionTemplate();

        Recipe existingRecipe = mock(Recipe.class);
        given(existingRecipe.getId()).willReturn(7L);
        RecipeYoutubeInfo existingInfo = mock(RecipeYoutubeInfo.class);
        given(existingInfo.getRecipe()).willReturn(existingRecipe);
        given(recipeYoutubeInfoRepository.findByVideoId(VIDEO_ID)).willReturn(Optional.of(existingInfo));

        facade.processYoutubeExtractionAsync(jobId, VALID_URL, VALID_MODEL, USER_ID);

        // dev V3 분기: quotaUsedOn 있음 → date-aware overload로 차감일에 환불 (cross-midnight 정확)
        verify(devYoutubeQuotaService).refund(USER_ID, DevYoutubeQuotaService.COST_BASIC, consumedOn);
        // deprecated overload는 호출 안 됨 (legacy fallback 우회 확인)
        verify(devYoutubeQuotaService, never()).refund(eq(USER_ID), eq(DevYoutubeQuotaService.COST_BASIC));
        verify(ytDlpService, never()).getVideoDataFull(any());
        assertThat(job.getTokenCost()).isZero();
    }


    @Test
    @DisplayName("dedup refund 성공 후 completeJob saveAndFlush 실패 → catch에서 managed.cost=0 보고 중복 환불 없음 (durable idempotency)")
    void async_dedupRefundIdempotency_noDoubleRefundOnCompleteFailure() {
        Long jobId = 101L;
        // 객체 분리: outer + tx별 managed entity가 별개 인스턴스 (real entity)
        RecipeGenerationJob outerJob = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC);
        RecipeGenerationJob managedForRefund = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC);
        RecipeGenerationJob managedForComplete = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC);
        // catch 안의 refundAndNullifyCost가 reload하면 DB 기준 cost=0이어야 함 (첫 refund tx commit 후 상태)
        RecipeGenerationJob managedAfterRefund = mockJobWithCost(jobId, 0);

        given(jobRepository.findById(jobId))
                .willReturn(Optional.of(outerJob))             // 1. async 진입
                .willReturn(Optional.of(managedForRefund))     // 2. refundAndNullifyCost 안 reload
                .willReturn(Optional.of(managedForComplete))   // 3. completeJob 안 reload
                .willReturn(Optional.of(managedAfterRefund));  // 4. catch 안 refundAndNullifyCost (DB는 이미 0)
        stubTransactionTemplate();

        Recipe existingRecipe = mock(Recipe.class);
        given(existingRecipe.getId()).willReturn(7L);
        RecipeYoutubeInfo existingInfo = mock(RecipeYoutubeInfo.class);
        given(existingInfo.getRecipe()).willReturn(existingRecipe);
        given(recipeYoutubeInfoRepository.findByVideoId(VIDEO_ID)).willReturn(Optional.of(existingInfo));

        // saveAndFlush 순서:
        //   #1 updateProgress(5%)                  → 성공
        //   #2 refundAndNullifyCost managed save   → 성공 → outer.setTokenCost(0)
        //   #3 completeJob save                    → THROW (테스트 대상)
        //   #4 handleAsyncError updateProgress     → 성공
        doAnswer(inv -> inv.getArgument(0))
                .doAnswer(inv -> inv.getArgument(0))
                .doAnswer(inv -> { throw new RuntimeException("DB down on completeJob"); })
                .doAnswer(inv -> inv.getArgument(0))
                .when(jobRepository).saveAndFlush(any(RecipeGenerationJob.class));

        facade.processYoutubeExtractionAsync(jobId, VALID_URL, VALID_MODEL, USER_ID);

        // refund DAO는 정확히 1회 — catch가 managed.cost=0 보고 no-op (durable idempotency)
        verify(devYoutubeQuotaService, times(1)).refund(USER_ID, DevYoutubeQuotaService.COST_BASIC);
        assertThat(managedForRefund.getTokenCost()).isZero();
        assertThat(outerJob.getTokenCost()).isZero();
    }

    @Test
    @DisplayName("durable idempotency: outer.cost=2 (stale) but DB managed.cost=0 → refund 호출 안 됨 (managed가 source of truth)")
    void async_outerStale_managedAlreadyZero_noRefund() {
        Long jobId = 104L;
        // outer는 cost=2 (stale 가정)
        RecipeGenerationJob outerJob = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC);
        // tx 안 reload한 managed는 cost=0 (DB 기준 이미 환불됨)
        RecipeGenerationJob managedAlreadyZero = mockJobWithCost(jobId, 0);

        given(jobRepository.findById(jobId))
                .willReturn(Optional.of(outerJob))            // 1. async 진입
                .willReturn(Optional.of(managedAlreadyZero)); // 2. refund tx 안 reload (DB는 이미 0)
        stubTransactionTemplate();

        Recipe existingRecipe = mock(Recipe.class);
        given(existingRecipe.getId()).willReturn(7L);
        RecipeYoutubeInfo existingInfo = mock(RecipeYoutubeInfo.class);
        given(existingInfo.getRecipe()).willReturn(existingRecipe);
        given(recipeYoutubeInfoRepository.findByVideoId(VIDEO_ID)).willReturn(Optional.of(existingInfo));

        // 모든 saveAndFlush 성공
        doAnswer(inv -> inv.getArgument(0))
                .when(jobRepository).saveAndFlush(any(RecipeGenerationJob.class));

        facade.processYoutubeExtractionAsync(jobId, VALID_URL, VALID_MODEL, USER_ID);

        // managed가 cost=0이라 tx 안에서 early return → refund DAO 절대 호출 안 됨
        verify(devYoutubeQuotaService, org.mockito.Mockito.never())
                .refund(any(), org.mockito.ArgumentMatchers.anyInt());
    }

    @Test
    @DisplayName("refundAndNullifyCost tx save 실패 → outer.tokenCost 원본 유지 → catch에서 정확히 1회 추가 refund")
    void async_refundTxFails_outerStateIntact_catchRetries() {
        Long jobId = 103L;
        // outer는 별개 인스턴스 — tx 실패 시 외부 setter가 호출 안 되어야 원본 cost 유지됨
        RecipeGenerationJob outerJob = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC);
        RecipeGenerationJob managedForFirst = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC);
        RecipeGenerationJob managedForSecond = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC);

        given(jobRepository.findById(jobId))
                .willReturn(Optional.of(outerJob))           // 1. async 진입
                .willReturn(Optional.of(managedForFirst))    // 2. dedup refund tx (실패)
                .willReturn(Optional.of(managedForSecond));  // 3. catch refund tx (재시도)
        stubTransactionTemplate();

        Recipe existingRecipe = mock(Recipe.class);
        given(existingRecipe.getId()).willReturn(7L);
        RecipeYoutubeInfo existingInfo = mock(RecipeYoutubeInfo.class);
        given(existingInfo.getRecipe()).willReturn(existingRecipe);
        given(recipeYoutubeInfoRepository.findByVideoId(VIDEO_ID)).willReturn(Optional.of(existingInfo));

        // saveAndFlush 순서:
        //   #1 updateProgress(5%)                       → 성공
        //   #2 첫 refund tx save (managedForFirst)      → THROW (tx rollback, outer setter 미실행)
        //   #3 catch handleAsyncError updateProgress    → 성공
        //   #4 catch refund tx save (managedForSecond)  → 성공 (재시도)
        doAnswer(inv -> inv.getArgument(0))
                .doAnswer(inv -> { throw new RuntimeException("refund save fail"); })
                .doAnswer(inv -> inv.getArgument(0))
                .doAnswer(inv -> inv.getArgument(0))
                .when(jobRepository).saveAndFlush(any(RecipeGenerationJob.class));

        facade.processYoutubeExtractionAsync(jobId, VALID_URL, VALID_MODEL, USER_ID);

        // refund DAO 정확히 2회 — 첫 시도 실패 + catch 재시도 (outer 원본 유지 덕분)
        // (만약 fix가 잘못되어 outer가 첫 tx 안에서 0으로 mutated되면 catch가 no-op하여 1회만 호출됨)
        verify(devYoutubeQuotaService, times(2)).refund(USER_ID, DevYoutubeQuotaService.COST_BASIC);
        // 최종적으로 두 번째 tx 성공 → outer 동기화
        assertThat(outerJob.getTokenCost()).isZero();
    }

    @Test
    @DisplayName("Grok garbage → Gemini fallback → date-aware upgrade(+3, usedOn) + 최종 cost 5로 date-aware refund (cross-midnight 정확)")
    @SuppressWarnings("deprecation")
    void async_grokGarbage_fallsBackToGeminiAndChargesUpgrade() {
        Long jobId = 102L;
        LocalDate consumedOn = LocalDate.of(2026, 4, 25); // 어제 시작 차감 시나리오
        RecipeGenerationJob job = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC, consumedOn);
        given(jobRepository.findById(jobId)).willReturn(Optional.of(job));
        stubTransactionTemplate();

        // Dedup 모두 empty (실제 추출로 진행)
        given(recipeYoutubeInfoRepository.findByVideoId(VIDEO_ID)).willReturn(Optional.empty());
        given(recipeRepository.findFirstOfficialByYoutubeUrl(any(), any())).willReturn(Optional.empty());

        // yt-dlp 메타데이터 (canonicalUrl은 빈 값으로 → 추가 dedup skip)
        YtDlpService.YoutubeFullDataDto data = new YtDlpService.YoutubeFullDataDto(
                VIDEO_ID, "", "title", "desc",
                "comments with sufficient ingredient keywords",
                "scriptTimecoded", "scriptPlain that is long enough to detect signal",
                "channel", "channelId", "thumbnail", "profile", 100L, 200L, 300L);
        given(ytDlpService.getVideoDataFull(VALID_URL)).willReturn(data);

        // 신호 충분 → Grok 시도 경로로 진입
        YoutubeSignalDetector.SignalReport signals =
                new YoutubeSignalDetector.SignalReport(true, true, false);
        given(signalDetector.detectSignals(any(), any(), any())).willReturn(signals);

        // Grok = garbage (재료 1개 → size < 2)
        RecipeCreateRequestDto garbageDto = new RecipeCreateRequestDto();
        garbageDto.setIsRecipe(true);
        garbageDto.setIngredients(List.of(new RecipeIngredientRequestDto()));
        given(grokClientService.generateRecipeStep1(any(), any()))
                .willReturn(CompletableFuture.completedFuture(garbageDto));

        // Gemini = valid (3 valid ingredients)
        RecipeCreateRequestDto validDto = new RecipeCreateRequestDto();
        validDto.setIsRecipe(true);
        validDto.setTitle("Valid recipe from Gemini");
        validDto.setIngredients(List.of(
                ing("재료1", "1", "g"),
                ing("재료2", "2", "큰술"),
                ing("재료3", "100", "ml")));
        given(geminiMultimodalService.generateRecipeFromYoutubeUrl(any(), any(), any()))
                .willReturn(CompletableFuture.completedFuture(validDto));

        // 이미지 라우터: 빈 리스트 (이미지 생성 skip, 흐름 계속)
        given(devImageGenRouterService.generate(any(), any(), any(), any()))
                .willReturn(List.of());

        // saveRecipeAndMetadataAtomic을 throw하게 만들어 short-circuit → catch 진입 → refund 분기 검증
        // (upgrade tx는 이미 성공 → managed.cost=5 → catch refund는 cost=5로 진행)
        doAnswer(inv -> { throw new RuntimeException("save failed for test"); })
                .when(transactionTemplate).execute(any());

        // when
        facade.processYoutubeExtractionAsync(jobId, VALID_URL, VALID_MODEL, USER_ID);

        // then — Gemini 경로가 선택되었음 (garbage check가 fallback 트리거)
        verify(geminiMultimodalService).generateRecipeFromYoutubeUrl(any(), any(), any());

        // upgrade는 date-aware overload로 (cross-midnight 정확성)
        verify(devYoutubeQuotaService).chargeGeminiUpgrade(USER_ID, consumedOn);
        // deprecated 1-arg overload는 호출 안 됨
        verify(devYoutubeQuotaService, never()).chargeGeminiUpgrade(USER_ID);

        // job state: upgrade tx 성공 → cost=5, usedGemini=true (이후 refund tx에서 cost=0으로 nullify)
        assertThat(job.getUsedGeminiAnalysis()).isTrue();
        assertThat(job.getTokenCost()).isZero();

        // 최종 환불은 date-aware overload + cost=5 + usedOn=consumedOn (시작일 row에 정확히 환불)
        verify(devYoutubeQuotaService).refund(USER_ID, DevYoutubeQuotaService.COST_WITH_GEMINI, consumedOn);
        // deprecated 2-arg overload는 호출 안 됨
        verify(devYoutubeQuotaService, never()).refund(eq(USER_ID), eq(DevYoutubeQuotaService.COST_WITH_GEMINI));
        verify(devYoutubeQuotaService, never()).refund(eq(USER_ID), eq(DevYoutubeQuotaService.COST_BASIC));
    }

    @Test
    @DisplayName("upgrade tx atomicity: chargeGeminiUpgrade 호출 후 managed save 실패 → outer.cost 원본 유지 → catch는 BASIC만 환불 (+3 누수 없음)")
    @SuppressWarnings("deprecation")
    void async_upgradeTxFails_outerStateIntact_refundOnlyBasic() {
        Long jobId = 105L;
        LocalDate consumedOn = LocalDate.of(2026, 4, 25);
        // outer는 cost=2, quotaUsedOn=어제 — upgrade tx 실패 시 outer setter가 호출 안 되어야 원본 유지됨
        RecipeGenerationJob outerJob = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC, consumedOn);
        // upgrade tx 안 reload — 이 entity의 mutation은 saveAndFlush 실패로 DB에 반영 안 됨 (real DB)
        // 테스트에서는 in-memory에서 cost=5로 set되더라도 다음 findById가 fresh entity 반환하면 격리됨
        RecipeGenerationJob managedForUpgrade = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC, consumedOn);
        // catch refund tx 안 reload — DB는 cost=2 (upgrade tx rollback) → fresh entity로 시뮬레이션
        RecipeGenerationJob managedForCatchRefund = mockJobWithCost(jobId, DevYoutubeQuotaService.COST_BASIC, consumedOn);

        given(jobRepository.findById(jobId))
                .willReturn(Optional.of(outerJob))             // 1. async 진입
                .willReturn(Optional.of(managedForUpgrade))    // 2. upgrade tx 안 reload (실패)
                .willReturn(Optional.of(managedForCatchRefund)); // 3. catch refund tx 안 reload (DB는 cost=2)
        stubTransactionTemplate();

        // Dedup 모두 empty (실제 추출로 진행)
        given(recipeYoutubeInfoRepository.findByVideoId(VIDEO_ID)).willReturn(Optional.empty());
        given(recipeRepository.findFirstOfficialByYoutubeUrl(any(), any())).willReturn(Optional.empty());

        YtDlpService.YoutubeFullDataDto data = new YtDlpService.YoutubeFullDataDto(
                VIDEO_ID, "", "title", "desc",
                "comments with sufficient ingredient keywords",
                "scriptTimecoded", "scriptPlain that is long enough to detect signal",
                "channel", "channelId", "thumbnail", "profile", 100L, 200L, 300L);
        given(ytDlpService.getVideoDataFull(VALID_URL)).willReturn(data);

        YoutubeSignalDetector.SignalReport signals =
                new YoutubeSignalDetector.SignalReport(true, true, false);
        given(signalDetector.detectSignals(any(), any(), any())).willReturn(signals);

        // Grok = garbage → Gemini fallback
        RecipeCreateRequestDto garbageDto = new RecipeCreateRequestDto();
        garbageDto.setIsRecipe(true);
        garbageDto.setIngredients(List.of(new RecipeIngredientRequestDto()));
        given(grokClientService.generateRecipeStep1(any(), any()))
                .willReturn(CompletableFuture.completedFuture(garbageDto));

        RecipeCreateRequestDto validDto = new RecipeCreateRequestDto();
        validDto.setIsRecipe(true);
        validDto.setTitle("Valid recipe from Gemini");
        validDto.setIngredients(List.of(
                ing("재료1", "1", "g"),
                ing("재료2", "2", "큰술"),
                ing("재료3", "100", "ml")));
        given(geminiMultimodalService.generateRecipeFromYoutubeUrl(any(), any(), any()))
                .willReturn(CompletableFuture.completedFuture(validDto));

        // saveAndFlush 순서:
        //   #1 updateProgress(5%)                            → 성공
        //   #2 updateProgress(20%)                           → 성공
        //   #3 updateProgress(40%)                           → 성공
        //   #4 upgrade tx save (managedForUpgrade)           → THROW (real DB라면 +3 차감도 rollback)
        //   #5 catch updateProgress(FAILED, 0)               → 성공
        //   #6 catch refund tx save (managedForCatchRefund)  → 성공
        doAnswer(inv -> inv.getArgument(0))
                .doAnswer(inv -> inv.getArgument(0))
                .doAnswer(inv -> inv.getArgument(0))
                .doAnswer(inv -> { throw new RuntimeException("upgrade save fail"); })
                .doAnswer(inv -> inv.getArgument(0))
                .doAnswer(inv -> inv.getArgument(0))
                .when(jobRepository).saveAndFlush(any(RecipeGenerationJob.class));

        // when
        facade.processYoutubeExtractionAsync(jobId, VALID_URL, VALID_MODEL, USER_ID);

        // then
        // upgrade DAO call은 발생함 (real DB라면 같은 tx에서 rollback되어 +3 차감도 무효)
        verify(devYoutubeQuotaService).chargeGeminiUpgrade(USER_ID, consumedOn);

        // outer setter는 tx 성공 후에만 실행 → tx 실패했으므로 outer는 BASIC 그대로
        // (이게 핵심 invariant: outer가 5로 mutated되면 catch는 5를 환불해서 +3 환불 누수 발생)
        // 단, refundAndNullifyCost 후에는 setTokenCost(0)이 호출됨 → 최종 0
        // 중간 상태(cost=BASIC vs cost=5)는 catch refund 시점에 managed로 결정됨
        // → managedForCatchRefund.cost=BASIC=2 이라 refund 호출은 (USER_ID, 2, consumedOn)
        verify(devYoutubeQuotaService).refund(USER_ID, DevYoutubeQuotaService.COST_BASIC, consumedOn);

        // 핵심 invariant: COST_WITH_GEMINI(5)로는 절대 환불 안 됨 (+3 누수 차단)
        verify(devYoutubeQuotaService, never())
                .refund(eq(USER_ID), eq(DevYoutubeQuotaService.COST_WITH_GEMINI), any(LocalDate.class));
        verify(devYoutubeQuotaService, never())
                .refund(eq(USER_ID), eq(DevYoutubeQuotaService.COST_WITH_GEMINI));
        // deprecated overload도 호출 안 됨
        verify(devYoutubeQuotaService, never()).refund(eq(USER_ID), eq(DevYoutubeQuotaService.COST_BASIC));
    }

    private RecipeIngredientRequestDto ing(String name, String quantity, String unit) {
        RecipeIngredientRequestDto i = new RecipeIngredientRequestDto();
        i.setName(name);
        i.setQuantity(quantity);
        i.setCustomUnit(unit);
        return i;
    }
}
