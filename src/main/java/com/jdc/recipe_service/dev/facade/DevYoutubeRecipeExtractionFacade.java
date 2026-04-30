package com.jdc.recipe_service.dev.facade;

import com.jdc.recipe_service.dev.domain.type.image.DevImageGenModel;
import com.jdc.recipe_service.dev.service.image.DevImageGenRouterService;
import com.jdc.recipe_service.dev.service.quota.DevYoutubeQuotaService;
import com.jdc.recipe_service.dev.service.youtube.YoutubeSignalDetector;
import com.jdc.recipe_service.dev.service.youtube.YoutubeSignalDetector.SignalReport;
import com.jdc.recipe_service.dev.util.youtube.YoutubeExtractionHelpers;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeExtractionInfo;
import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeInfo;
import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.meta.RecipeYoutubeExtractionInfoRepository;
import com.jdc.recipe_service.domain.repository.meta.RecipeYoutubeInfoRepository;
import com.jdc.recipe_service.domain.type.JobStatus;
import com.jdc.recipe_service.domain.type.JobType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.media.EvidenceLevel;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeExtractionService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.ai.GeminiMultimodalService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import com.jdc.recipe_service.service.media.YtDlpService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.LocalDate;
import java.util.HexFormat;
import java.util.List;
import java.util.Optional;

/**
 * Dev V3 YouTube extraction facade.
 *
 * 운영 V2 (RecipeExtractionService.createYoutubeExtractionJobV2 / processYoutubeExtractionAsyncV2) 미러 +
 * dev 신규 기능:
 *  - imageGenModel 화이트리스트 + image_generation_model 컬럼 저장
 *  - DevYoutubeQuotaService 변동 비용 (BASIC=2, GEMINI 사용 시 +3 = 5)
 *  - YoutubeSignalDetector로 신호 검출 + EvidenceLevel 산출
 *  - RecipeYoutubeExtractionInfo 저장 (signals + evidence + cost)
 *  - RecipeYoutubeInfo dual-write (legacy Recipe.youtube* 와 함께)
 *  - Recipe.applyVisibility() 트리플 동기화
 *  - Idempotency key namespacing (dev-yt:{userId}:{sha256(rawKey)})
 *
 * 본 MVP에서 생략 (V2에 있지만 dev 1차에는 미포함, 후속 단계):
 *  - bus 패턴 (동일 videoId 동시 추출 dedup)
 *  - grokClientService.refineIngredientsOnly (재료 정제 추가 호출)
 *  - mergeDuplicateIngredientsByNameAndUnit
 *  - addFavoriteToUser (즐겨찾기 자동 등록)
 *  - recipeActivityService log
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevYoutubeRecipeExtractionFacade {

    public record JobCreateResult(Long jobId, boolean created) {}

    /** YouTube 추출은 official 계정 소유 (V2와 동일). 요청자는 extractorId로 별도 기록. */
    private static final Long OFFICIAL_RECIPE_USER_ID = 90121L;
    private static final String DEV_YT_NAMESPACE = "dev-yt:";

    private final YtDlpService ytDlpService;
    private final GrokClientService grokClientService;
    private final GeminiMultimodalService geminiMultimodalService;
    private final RecipeService recipeService;
    private final AsyncImageService asyncImageService;

    private final DevYoutubeQuotaService devYoutubeQuotaService;
    private final DevImageGenRouterService devImageGenRouterService;
    private final YoutubeSignalDetector signalDetector;

    private final RecipeRepository recipeRepository;
    private final RecipeGenerationJobRepository jobRepository;
    private final RecipeYoutubeInfoRepository recipeYoutubeInfoRepository;
    private final RecipeYoutubeExtractionInfoRepository extractionInfoRepository;
    private final com.jdc.recipe_service.dev.service.recipe.ingredient.DevRecipeIngredientPersistService devIngredientPersist;

    private final TransactionTemplate transactionTemplate;

    // =========================================================================
    // Phase 1: 작업 접수
    // =========================================================================

    /**
     * @return JobCreateResult — created=false면 idempotency 재요청, controller에서 async 호출 금지.
     */
    @Transactional
    public JobCreateResult createYoutubeExtractionJob(String videoUrl,
                                                      String imageGenModel,
                                                      Long userId,
                                                      String idempotencyKey) {
        // 1) 도메인 패턴 + 2) videoId 추출 가능성까지 사전 검증.
        // (channel/playlist URL이 도메인은 youtube.com이라 1단계는 통과할 수 있으므로 2단계 필수.)
        if (videoUrl == null || !YoutubeExtractionHelpers.YOUTUBE_URL_PATTERN.matcher(videoUrl).matches()) {
            throw new CustomException(ErrorCode.INVALID_URL_FORMAT);
        }
        String videoId = YoutubeExtractionHelpers.extractVideoId(videoUrl);
        if (!YoutubeExtractionHelpers.isValidVideoId(videoId)) {
            // channel page (/@channel), 빈 v= 등 → 추출 가능한 video ID 없음 → 쿼터 차감 전에 거부
            throw new CustomException(ErrorCode.INVALID_URL_FORMAT);
        }

        // 화이트리스트 검증 — UNSUPPORTED_IMAGE_MODEL(703) → 400
        DevImageGenModel.fromIdentifier(imageGenModel);

        String storedKey = namespacedIdempotencyKey(userId, idempotencyKey);

        Optional<RecipeGenerationJob> existing = jobRepository.findByIdempotencyKey(storedKey);
        if (existing.isPresent()) {
            log.info("♻️ [DevYt V3] 기존 작업 재사용 - storedKey={}, jobId={}, status={}",
                    storedKey, existing.get().getId(), existing.get().getStatus());
            return new JobCreateResult(existing.get().getId(), false);
        }

        // 시작 비용(2) 차감 — 실패 시 DailyQuotaExceededException → 429.
        // 차감 시점 날짜를 잡아 job에 저장 → cross-midnight 환불 정확성 보장 (자정 넘어 실패해도 시작일 quota에서 환불).
        LocalDate quotaUsedOn = devYoutubeQuotaService.tryStartOrThrow(userId);

        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .userId(userId)
                .jobType(JobType.YOUTUBE_EXTRACTION)
                .status(JobStatus.PENDING)
                .progress(0)
                .idempotencyKey(storedKey)
                .imageGenerationModel(imageGenModel)
                .tokenCost(DevYoutubeQuotaService.COST_BASIC)
                .usedGeminiAnalysis(false)
                .quotaUsedOn(quotaUsedOn)
                .build();

        jobRepository.saveAndFlush(job);

        log.info("🆕 [DevYt V3] 신규 작업 생성 - jobId={}, imageGenModel={}, storedKey={}",
                job.getId(), imageGenModel, storedKey);
        return new JobCreateResult(job.getId(), true);
    }

    // =========================================================================
    // Phase 2: 비동기 본 처리
    // =========================================================================

    @Async("recipeExtractionExecutor")
    public void processYoutubeExtractionAsync(Long jobId,
                                              String videoUrl,
                                              String imageGenModel,
                                              Long userId) {
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        if (job.getStatus() == JobStatus.COMPLETED) return;

        String videoId = YoutubeExtractionHelpers.extractVideoId(videoUrl);
        if (videoId == null || videoId.isBlank()) {
            handleAsyncError(job, userId, new CustomException(ErrorCode.INVALID_URL_FORMAT));
            return;
        }

        try {
            updateProgress(job, JobStatus.IN_PROGRESS, 5);

            // 1a. RecipeYoutubeInfo.video_id 기반 dedup (분리 테이블의 unique 제약 = 가장 정확한 키)
            Optional<RecipeYoutubeInfo> existingByVideoId = recipeYoutubeInfoRepository.findByVideoId(videoId);
            if (existingByVideoId.isPresent()) {
                Long existingRecipeId = existingByVideoId.get().getRecipe().getId();
                log.info("♻️ [DevYt V3] video_id로 기존 발견. 환불({}) + 완료. recipeId={}",
                        job.getTokenCost(), existingRecipeId);
                refundAndNullifyCost(job, userId); // idempotent
                completeJob(jobId, existingRecipeId);
                return;
            }

            // 1b. legacy Recipe.youtube_url 기반 dedup (V1/V2가 저장한 row 호환)
            Optional<Recipe> existingByUrl = findExistingByUrl(videoId);
            if (existingByUrl.isPresent()) {
                Recipe existing = existingByUrl.get();
                log.info("♻️ [DevYt V3] legacy URL로 기존 official recipe 발견. 환불({}) + 완료. recipeId={}",
                        job.getTokenCost(), existing.getId());
                refundAndNullifyCost(job, userId); // idempotent
                completeJob(jobId, existing.getId());
                return;
            }

            // 2. yt-dlp 메타데이터
            updateProgress(job, JobStatus.IN_PROGRESS, 20);
            YtDlpService.YoutubeFullDataDto videoData = ytDlpService.getVideoDataFull(videoUrl);

            String description = YoutubeExtractionHelpers.cap(
                    YoutubeExtractionHelpers.nullToEmpty(videoData.description()),
                    YoutubeExtractionHelpers.MAX_DESC_CHARS);
            String comments = YoutubeExtractionHelpers.cap(
                    YoutubeExtractionHelpers.nullToEmpty(videoData.comments()),
                    YoutubeExtractionHelpers.MAX_CMT_CHARS);
            String scriptPlain = YoutubeExtractionHelpers.cap(
                    YoutubeExtractionHelpers.nullToEmpty(videoData.scriptPlain()),
                    YoutubeExtractionHelpers.MAX_SCRIPT_CHARS);
            String title = YoutubeExtractionHelpers.nullToEmpty(videoData.title());

            // 3. 신호 검출 (이 시점에 잠가둠 — Gemini 결정 후 evidence 산출에 사용)
            SignalReport signals = signalDetector.detectSignals(description, comments, scriptPlain);

            // 4. canonical URL 기반 dedup 한 번 더 (yt-dlp 응답 후)
            String canonicalUrl = YoutubeExtractionHelpers.nullToEmpty(videoData.canonicalUrl());
            if (!canonicalUrl.isBlank()) {
                Optional<Recipe> existingByCanonical = recipeRepository
                        .findFirstOfficialByYoutubeUrl(canonicalUrl, OFFICIAL_RECIPE_USER_ID);
                if (existingByCanonical.isPresent()) {
                    log.info("♻️ [DevYt V3] canonical URL로 기존 발견. 환불 + 완료.");
                    refundAndNullifyCost(job, userId); // idempotent
                    completeJob(jobId, existingByCanonical.get().getId());
                    return;
                }
            }

            // 5. Grok vs Gemini 결정 + 추출
            updateProgress(job, JobStatus.IN_PROGRESS, 40);
            ExtractionResult result = runExtraction(
                    videoUrl, videoId, title, description, comments, scriptPlain, videoData.duration(), signals);

            // 6. Gemini 사용 시 +3 추가 차감 + job tokenCost 갱신 (atomic)
            if (result.usedGemini()) {
                chargeGeminiUpgradeAtomic(jobId, userId, job);
            }

            // 7. 이미지 생성 — DevImageGenRouterService로 모델 라우팅
            updateProgress(job, JobStatus.IN_PROGRESS, 60);
            String imageUrl = generateImageWithRouter(result.recipeDto(), imageGenModel, videoId);
            applyImageGenerationResult(result.recipeDto(), imageUrl);

            // 8. legacy Recipe.youtube* 필드 dual-write (운영 V1/V2 호환)
            applyYoutubeFieldsToDto(result.recipeDto(), videoId, videoData);

            // 9. Recipe 저장 + metadata를 단일 트랜잭션으로 묶음 (atomicity)
            //    metadata 저장 실패(예: video_id unique 위반) 시 Recipe도 함께 rollback → orphan 방지
            updateProgress(job, JobStatus.IN_PROGRESS, 85);
            Long recipeId = saveRecipeAndMetadataAtomic(
                    result.recipeDto(), userId, videoId, videoData, signals, result.usedGemini(),
                    job.getTokenCost(), imageGenModel);

            completeJob(jobId, recipeId);

            log.info("✅ [DevYt V3] 추출 완료 - jobId={}, recipeId={}, evidence={}, cost={}, usedGemini={}",
                    jobId, recipeId,
                    signalDetector.computeEvidence(signals, result.usedGemini()),
                    job.getTokenCost(), result.usedGemini());

        } catch (Exception e) {
            handleAsyncError(job, userId, e);
        }
    }

    // =========================================================================
    // Phase 3: 상태 조회
    // =========================================================================

    @Transactional(readOnly = true)
    public JobStatusDto getJobStatus(Long jobId) {
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        Long recipeId = job.getResultRecipeId();
        if (job.getStatus() == JobStatus.COMPLETED && recipeId != null
                && !recipeRepository.existsById(recipeId)) {
            recipeId = null;
        }

        String code = null;
        String msg = job.getErrorMessage();
        if (job.getStatus() == JobStatus.FAILED && msg != null && msg.contains("::")) {
            String[] parts = msg.split("::", 2);
            code = parts[0];
            msg = parts[1];
        }

        return JobStatusDto.builder()
                .jobId(job.getId())
                .status(job.getStatus())
                .resultRecipeId(recipeId)
                .code(code)
                .message(msg)
                .progress(job.getProgress())
                .build();
    }

    // =========================================================================
    // private helpers
    // =========================================================================

    private record ExtractionResult(RecipeCreateRequestDto recipeDto, boolean usedGemini) {}

    /**
     * Grok 우선 시도, 실패/부족 시 Gemini Multimodal fallback.
     * V2 isTextSufficient 분기와 동일 정책.
     */
    private ExtractionResult runExtraction(String videoUrl, String videoId, String title,
                                           String description, String comments, String scriptPlain,
                                           Long videoDuration, SignalReport signals) {
        boolean useFallback = false;
        RecipeCreateRequestDto dto = null;

        String fullContext = YoutubeExtractionHelpers.cap(("""
                영상 URL: %s
                영상 제목: %s
                영상 설명: %s
                고정/인기 댓글: %s
                자막: %s
                """).formatted(
                YoutubeExtractionHelpers.buildStorageYoutubeUrl(videoId, false),
                title,
                YoutubeExtractionHelpers.emptyToPlaceholder(description, "(없음)"),
                YoutubeExtractionHelpers.emptyToPlaceholder(comments, "(없음)"),
                YoutubeExtractionHelpers.emptyToPlaceholder(scriptPlain, "(없음)")
        ), YoutubeExtractionHelpers.MAX_CONTEXT_CHARS);

        // 신호 충분 시 Grok 시도
        if (isTextSufficient(signals)) {
            try {
                dto = grokClientService.generateRecipeStep1(
                        RecipeExtractionService.getExtractionPrompt()
                                + com.jdc.recipe_service.dev.util.prompt.DevIngredientPromptOverride.SECTION,
                        fullContext).join();

                // 명시적 "레시피 아님" 판정은 즉시 거부
                if (dto != null && Boolean.FALSE.equals(dto.getIsRecipe())) {
                    throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                            "레시피 아님: " + dto.getNonRecipeReason());
                }

                // V2와 동일: isRecipe=true여도 결과가 garbage(재료 1개, 수량 이상)면 Gemini로 재시도
                // garbage 결과를 그대로 저장하면 evidence/cost가 잘못 기록됨 → 반드시 fallback
                if (YoutubeExtractionHelpers.isRecipeResultGarbage(dto)) {
                    log.warn("📉 [DevYt V3] Grok 결과 품질 미달 (garbage). Gemini fallback 진행");
                    useFallback = true;
                    dto = null;
                } else if (dto == null || !Boolean.TRUE.equals(dto.getIsRecipe())) {
                    useFallback = true;
                    dto = null;
                }
            } catch (CustomException e) {
                throw e;
            } catch (Exception e) {
                log.warn("⚠️ [DevYt V3] Grok 실패, Gemini fallback. error={}",
                        YoutubeExtractionHelpers.safeMsg(e));
                useFallback = true;
                dto = null;
            }
        } else {
            useFallback = true;
        }

        // Gemini fallback
        if (useFallback || dto == null) {
            if (videoDuration != null && videoDuration > YoutubeExtractionHelpers.MAX_VIDEO_DURATION_SECONDS) {
                throw new CustomException(ErrorCode.INVALID_INPUT_VALUE,
                        "텍스트 정보가 부족하며, 영상이 너무 길어(70분 초과) AI 심층 분석을 진행할 수 없습니다.");
            }
            String promptWithHint = RecipeExtractionService.getExtractionPrompt()
                    + com.jdc.recipe_service.dev.util.prompt.DevIngredientPromptOverride.SECTION
                    + "\n\n## [참고용 텍스트 데이터]\n" + fullContext;
            String storageUrl = YoutubeExtractionHelpers.buildStorageYoutubeUrl(videoId, false);
            dto = geminiMultimodalService.generateRecipeFromYoutubeUrl(promptWithHint, title, storageUrl).join();

            if (dto == null || !Boolean.TRUE.equals(dto.getIsRecipe())) {
                throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "레시피 아님/생성실패");
            }
            return new ExtractionResult(dto, true);
        }

        return new ExtractionResult(dto, false);
    }

    /**
     * V2 isTextSufficient 정책 단순 판정: signals만 기반.
     * (자막 OR 설명 OR 댓글 중 ingredient 신호가 있으면 Grok 시도 가치 있음.)
     */
    private boolean isTextSufficient(SignalReport signals) {
        return signals.hasSubtitle()
                || signals.hasDescriptionIngredient()
                || signals.hasCommentIngredient();
    }

    /**
     * 이미지 생성: AsyncImageService.buildPromptFromDto로 V2와 동일 prompt 빌드 후 router로 위임.
     * 실패 시 null 반환 (recipe는 default 이미지로 저장).
     */
    private String generateImageWithRouter(RecipeCreateRequestDto dto, String imageGenModel, String videoId) {
        try {
            String prompt = asyncImageService.buildPromptFromDto(dto);
            List<String> urls = devImageGenRouterService.generate(
                    imageGenModel, prompt, OFFICIAL_RECIPE_USER_ID, videoId);
            return urls.isEmpty() ? null : urls.get(0);
        } catch (Exception e) {
            log.warn("⚠️ [DevYt V3] 이미지 생성 실패 (텍스트만 저장). error={}",
                    YoutubeExtractionHelpers.safeMsg(e));
            return null;
        }
    }

    private void applyImageGenerationResult(RecipeCreateRequestDto dto, String imageUrl) {
        if (imageUrl != null && !imageUrl.isBlank()) {
            dto.setImageKey(YoutubeExtractionHelpers.extractS3Key(imageUrl));
            dto.setImageStatus(RecipeImageStatus.READY);
            return;
        }

        dto.setImageKey(AsyncImageService.DEFAULT_IMAGE_KEY);
        dto.setImageStatus(RecipeImageStatus.FAILED);
    }

    /**
     * legacy Recipe.youtube* 필드 dual-write — 운영 V1/V2와 호환을 위해 유지.
     * 향후 RecipeYoutubeInfo로 완전 이행 후 contract 단계에서 제거.
     */
    private void applyYoutubeFieldsToDto(RecipeCreateRequestDto dto, String videoId,
                                          YtDlpService.YoutubeFullDataDto videoData) {
        String storageUrl = YoutubeExtractionHelpers.buildStorageYoutubeUrl(videoId, false);
        dto.setYoutubeUrl(storageUrl);
        dto.setYoutubeChannelName(YoutubeExtractionHelpers.nullToEmpty(videoData.channelName()));
        dto.setYoutubeChannelId(YoutubeExtractionHelpers.nullToEmpty(videoData.channelId()));
        dto.setYoutubeVideoTitle(YoutubeExtractionHelpers.nullToEmpty(videoData.title()));
        dto.setYoutubeThumbnailUrl(YoutubeExtractionHelpers.nullToEmpty(videoData.thumbnailUrl()));
        dto.setYoutubeChannelProfileUrl(YoutubeExtractionHelpers.nullToEmpty(videoData.channelProfileUrl()));
        dto.setYoutubeSubscriberCount(videoData.youtubeSubscriberCount());
        dto.setYoutubeVideoViewCount(videoData.viewCount());
    }

    /**
     * URL 기반 기존 official recipe 조회. watch + shorts 두 형식 모두 시도.
     */
    private Optional<Recipe> findExistingByUrl(String videoId) {
        String watchUrl = YoutubeExtractionHelpers.buildStorageYoutubeUrl(videoId, false);
        String shortsUrl = YoutubeExtractionHelpers.buildStorageYoutubeUrl(videoId, true);
        return recipeRepository.findFirstOfficialByYoutubeUrl(watchUrl, OFFICIAL_RECIPE_USER_ID)
                .or(() -> recipeRepository.findFirstOfficialByYoutubeUrl(shortsUrl, OFFICIAL_RECIPE_USER_ID));
    }

    /**
     * Recipe + RecipeYoutubeInfo + RecipeYoutubeExtractionInfo를 단일 트랜잭션으로 저장.
     *
     * Atomicity 이유: metadata 저장 단계에서 video_id unique 위반(`uq_youtube_video_id`)이나
     * DB 오류가 나면 이미 저장된 Recipe가 orphan으로 남는 문제 방지. 여기서 throw되면
     * Recipe 저장도 함께 rollback되어 일관성 유지.
     *
     *  - owner=OFFICIAL_RECIPE_USER_ID, extractor=requester (V2와 동일)
     *  - recipe.imageGenerationModel + applyVisibility(PUBLIC) — 트리플 동기화
     *  - RecipeYoutubeInfo (분리 테이블)
     *  - RecipeYoutubeExtractionInfo (signals + evidence + cost)
     */
    private Long saveRecipeAndMetadataAtomic(RecipeCreateRequestDto recipeDto, Long extractorId,
                                              String videoId, YtDlpService.YoutubeFullDataDto videoData,
                                              SignalReport signals, boolean usedGemini, int finalCost,
                                              String imageGenModel) {
        return transactionTemplate.execute(status -> {
            // 1.4 empty-then-save: 운영 호출에는 ingredients=[]/step.ingredients=[] 전달, dev persist로 dual-write +
            // step relink. atomicity 유지 — 같은 transactionTemplate 안에서 모든 단계 실행.
            recipeDto.setExtractorId(extractorId);

            List<RecipeIngredientRequestDto> originalIngredients = recipeDto.getIngredients();
            List<RecipeStepRequestDto> originalSteps = recipeDto.getSteps();
            Integer suppliedMarketPrice = recipeDto.getMarketPrice();

            RecipeCreateRequestDto operationalDto = recipeDto.toBuilder()
                    .ingredients(List.of())
                    .steps(stripStepIngredients(originalSteps))
                    .build();

            RecipeWithImageUploadRequest request = new RecipeWithImageUploadRequest();
            request.setRecipe(operationalDto);
            PresignedUrlResponse response = recipeService.createRecipeAndGenerateUrls(
                    request, OFFICIAL_RECIPE_USER_ID, RecipeSourceType.YOUTUBE, null);
            Long recipeId = response.getRecipeId();

            // 2. 같은 tx 안에서 reload + metadata 적용
            Recipe recipe = recipeRepository.findById(recipeId)
                    .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

            recipe.updateImageGenerationModel(imageGenModel);
            recipe.applyVisibility(RecipeVisibility.PUBLIC);

            // 2a. dev ingredient dual-write + step→recipe_ingredient FK 재연결.
            // YouTube path → customByUser=false 강제 (시스템 매칭 실패는 UNRESOLVED이지 사용자 의도 CUSTOM 아님)
            devIngredientPersist.persistAllSystemSourced(recipe, originalIngredients, suppliedMarketPrice);
            devIngredientPersist.linkStepIngredients(recipe, originalSteps);

            // 3. RecipeYoutubeInfo (video_id unique 충돌 시 여기서 throw → 전체 rollback)
            RecipeYoutubeInfo youtubeInfo = RecipeYoutubeInfo.builder()
                    .recipe(recipe)
                    .videoId(videoId)
                    .youtubeUrl(YoutubeExtractionHelpers.buildStorageYoutubeUrl(videoId, false))
                    .channelName(YoutubeExtractionHelpers.nullToEmpty(videoData.channelName()))
                    .channelId(YoutubeExtractionHelpers.nullToEmpty(videoData.channelId()))
                    .videoTitle(YoutubeExtractionHelpers.nullToEmpty(videoData.title()))
                    .thumbnailUrl(YoutubeExtractionHelpers.nullToEmpty(videoData.thumbnailUrl()))
                    .channelProfileUrl(YoutubeExtractionHelpers.nullToEmpty(videoData.channelProfileUrl()))
                    .subscriberCount(videoData.youtubeSubscriberCount())
                    .videoViewCount(videoData.viewCount())
                    .extractorId(extractorId)
                    .durationSeconds(videoData.duration())
                    .build();
            recipeYoutubeInfoRepository.save(youtubeInfo);

            // 4. RecipeYoutubeExtractionInfo (signals + evidence + cost)
            EvidenceLevel evidence = signalDetector.computeEvidence(signals, usedGemini);
            RecipeYoutubeExtractionInfo extractionInfo = RecipeYoutubeExtractionInfo.builder()
                    .recipe(recipe)
                    .hasSubtitle(signals.hasSubtitle())
                    .hasDescriptionIngredient(signals.hasDescriptionIngredient())
                    .hasCommentIngredient(signals.hasCommentIngredient())
                    .usedGeminiAnalysis(usedGemini)
                    .evidenceLevel(evidence)
                    .tokenCost(finalCost)
                    .build();
            extractionInfoRepository.save(extractionInfo);

            return recipeId;
        });
    }

    /**
     * 운영 호출용 step DTO 클론 — 각 step의 ingredients만 빈 리스트로. 원본 step list는 mutate 안 함.
     * 운영 RecipeStepService.saveAll이 step ingredient를 만들지 않게 차단 → dev linkStepIngredients가
     * 다시 연결 (1.3 패턴과 동일).
     */
    private static List<RecipeStepRequestDto> stripStepIngredients(List<RecipeStepRequestDto> originalSteps) {
        if (originalSteps == null) return null;
        return originalSteps.stream()
                .map(s -> s == null ? null : RecipeStepRequestDto.builder()
                        .stepNumber(s.getStepNumber())
                        .instruction(s.getInstruction())
                        .imageKey(s.getImageKey())
                        .action(s.getAction())
                        .timeline(s.getTimeline())
                        .ingredients(List.of())
                        .build())
                .toList();
    }

    /**
     * 진행률 업데이트.
     *
     * 주의: 같은 클래스 내부에서 호출되어 AOP self-invocation으로 인해 `@Transactional(REQUIRES_NEW)`는
     * 효과가 없다. 그래서 명시적으로 빼두고, `jobRepository.saveAndFlush`가 자체 tx (Spring Data
     * Repository의 `@Transactional`)를 여는 것에 의존한다.
     *
     * 현재 async 경로에는 outer tx가 없어 이 saveAndFlush가 독립 tx처럼 즉시 commit된다.
     * 다만 향후 호출자가 tx를 두르면 repository의 tx는 join되므로, 진짜 REQUIRES_NEW가 필요하면
     * 별도 service로 분리해야 한다.
     */
    public void updateProgress(RecipeGenerationJob job, JobStatus status, int progress) {
        job.updateProgress(status, progress);
        jobRepository.saveAndFlush(job);
    }

    private void completeJob(Long jobId, Long resultRecipeId) {
        transactionTemplate.executeWithoutResult(status -> {
            RecipeGenerationJob job = jobRepository.findById(jobId)
                    .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));
            job.complete(resultRecipeId);
            jobRepository.saveAndFlush(job);
        });
    }

    private void handleAsyncError(RecipeGenerationJob job, Long userId, Exception e) {
        log.error("❌ [DevYt V3] 추출 실패 jobId={}, tokenCost={}", job.getId(), job.getTokenCost(), e);

        ErrorCode errorCode = (e instanceof CustomException ce)
                ? ce.getErrorCode()
                : ErrorCode.INTERNAL_SERVER_ERROR;
        String clientMsg = (e instanceof CustomException) ? e.getMessage() : "유튜브 추출 중 오류가 발생했습니다.";

        job.setErrorMessage(errorCode.getCode() + "::" + clientMsg);
        updateProgress(job, JobStatus.FAILED, 0);

        // "레시피 아님" 판정은 환불 안 함 (V2와 동일 정책)
        if (errorCode != ErrorCode.INVALID_INPUT_VALUE) {
            try {
                refundAndNullifyCost(job, userId); // idempotent — dedup에서 이미 환불됐다면 no-op
            } catch (Exception refundEx) {
                log.warn("⚠️ [DevYt V3] 쿼터 환불 실패: {}", refundEx.getMessage());
            }
        }
    }

    /**
     * Gemini upgrade 차감 + managed job tokenCost/usedGeminiAnalysis 갱신을 단일 tx로 묶는다.
     *
     * 원자성 이유:
     *  - chargeGeminiUpgrade DAO 호출 (JdbcTemplate) 후 saveAndFlush (JPA) 실패하면
     *    기존 구현은 DB job.tokenCost가 여전히 BASIC(2)인 상태로 남아 catch 환불이 2만 진행 → +3 누수.
     *  - 같은 DataSource를 공유하는 두 호출을 하나의 tx에 묶으면 save 실패 시 +3 차감도 함께 rollback.
     *
     * Source of truth: tx 안에서 reload한 managed entity의 quotaUsedOn을 기준으로 charge한다.
     *  - refundAndNullifyCost와 동일 패턴 — outer가 stale일 가능성을 차단.
     *  - charge 날짜와 환불 날짜가 항상 같은 DB row 기준으로 일치 (cross-midnight 누수 방지).
     */
    private void chargeGeminiUpgradeAtomic(Long jobId, Long userId, RecipeGenerationJob outerJob) {
        transactionTemplate.executeWithoutResult(status -> {
            RecipeGenerationJob managed = jobRepository.findById(jobId)
                    .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

            // managed.quotaUsedOn을 source of truth로 사용 — outer는 stale 가능.
            // null(legacy job)이면 service 내부에서 deprecated today 경로로 fallback.
            LocalDate quotaUsedOn = managed.getQuotaUsedOn();
            devYoutubeQuotaService.chargeGeminiUpgrade(userId, quotaUsedOn);

            managed.setTokenCost(DevYoutubeQuotaService.COST_WITH_GEMINI);
            managed.setUsedGeminiAnalysis(true);
            jobRepository.saveAndFlush(managed);
        });
        // tx 성공 후만 outer 동기화 — tx rollback 시 outer 원본 유지 (이후 catch refund가 BASIC만 환불 → 누수 0)
        outerJob.setTokenCost(DevYoutubeQuotaService.COST_WITH_GEMINI);
        outerJob.setUsedGeminiAnalysis(true);
    }

    /**
     * 환불 + tokenCost=0 nullify를 단일 트랜잭션으로 묶어 durable idempotency 보장.
     *
     * 핵심: tx 안에서 reload한 managed entity의 tokenCost를 **source of truth**로 삼는다.
     *  - outer job은 stale일 수 있음 (다른 흐름이 DB를 0으로 바꿨을 수도, mock/테스트 환경 등).
     *  - 결정/refund/save 모두 managed 기준으로 → 중복 환불 원천 차단 (durable idempotency).
     *  - tx 성공 후에만 outer 객체 동기화 → tx rollback 시 outer 원본 유지 → catch에서 재시도 가능.
     *
     * 두 작업(refund + save)이 같은 tx에 묶이는 이유:
     *  - DAO refund (JdbcTemplate)와 jobRepository.saveAndFlush (JPA)가 같은 DataSource 공유 →
     *    Spring tx에서 함께 commit/rollback. saveAndFlush 실패 시 refund도 rollback.
     *
     * dedup 환불(success path)과 handleAsyncError 환불(failure path) 양쪽에서 사용.
     */
    @SuppressWarnings("deprecation") // legacy fallback (quotaUsedOn=null인 row)에서 deprecated refund 사용
    private void refundAndNullifyCost(RecipeGenerationJob job, Long userId) {
        Long jobId = job.getId();
        transactionTemplate.executeWithoutResult(status -> {
            // tx 안 reload한 managed가 source of truth — outer가 stale이어도 DB 상태로 결정
            RecipeGenerationJob managed = jobRepository.findById(jobId)
                    .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

            Integer managedCost = managed.getTokenCost();
            if (managedCost == null || managedCost <= 0) {
                // DB 기준 이미 환불됨 → no-op (중복 환불 방지)
                return;
            }

            // cross-midnight 환불 정확성: 차감 시점 날짜로 환불 (managed.quotaUsedOn).
            // legacy job (V20260426_006 마이그레이션 이전에 생성된 row)은 quotaUsedOn=null →
            // deprecated overload로 today 환불 (V2 호환). 신규 dev V3 job은 항상 quotaUsedOn 채워짐.
            LocalDate quotaUsedOn = managed.getQuotaUsedOn();
            if (quotaUsedOn != null) {
                devYoutubeQuotaService.refund(userId, managedCost, quotaUsedOn);
            } else {
                devYoutubeQuotaService.refund(userId, managedCost);
            }
            managed.setTokenCost(0);
            jobRepository.saveAndFlush(managed);
        });
        // tx 성공 후에만 outer 동기화 — tx 실패 시 outer는 원본 유지 → catch retry 정확
        job.setTokenCost(0);
    }

    // ---------- idempotency key namespacing ----------

    private static String namespacedIdempotencyKey(Long userId, String clientKey) {
        return DEV_YT_NAMESPACE + userId + ":" + sha256Hex(clientKey == null ? "" : clientKey);
    }

    private static String sha256Hex(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            return HexFormat.of().formatHex(md.digest(input.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException("SHA-256 algorithm unavailable", e);
        }
    }
}
