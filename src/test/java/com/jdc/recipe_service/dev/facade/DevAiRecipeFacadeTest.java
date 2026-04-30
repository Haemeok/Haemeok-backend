package com.jdc.recipe_service.dev.facade;

import com.jdc.recipe_service.dev.facade.DevAiRecipeFacade.JobCreateResult;
import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.QuotaType;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.DailyQuotaService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.SurveyService;
import com.jdc.recipe_service.service.ai.GeminiClientService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import com.jdc.recipe_service.util.ActionImageService;
import com.jdc.recipe_service.util.prompt.CostEffectivePromptBuilder;
import com.jdc.recipe_service.util.prompt.FineDiningPromptBuilder;
import com.jdc.recipe_service.util.prompt.IngredientFocusPromptBuilder;
import com.jdc.recipe_service.util.prompt.NutritionPromptBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.TransactionCallback;
import org.springframework.transaction.support.TransactionTemplate;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HexFormat;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.function.Consumer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * DevAiRecipeFacade 단위 테스트.
 *
 * 검증 포인트 (리뷰 피드백 기반):
 *  1. 동일 idempotencyKey 재요청 시 created=false 반환 → controller가 async 재실행 안 하도록 보장
 *  2. consume이 token으로 fallback 됐는지 정확히 캡처해서 JobCreateResult.usedToken에 반영
 *  3. async 실패 시 refund가 usedToken 인자를 그대로 사용 (V2의 항상 true 환불 버그 회피)
 *  4. imageGenModel 화이트리스트 위반은 쿼터 차감 전에 차단
 *  5. imageGenModel이 RecipeGenerationJob에 저장됨
 *  6. idempotency 키는 "userId:rawKey"로 namespacing되어 저장/조회 (cross-user 충돌 방지)
 */
@ExtendWith(MockitoExtension.class)
class DevAiRecipeFacadeTest {

    @Mock RecipeRepository recipeRepository;
    @Mock RecipeGenerationJobRepository jobRepository;
    @Mock GrokClientService grokClientService;
    @Mock GeminiClientService geminiClientService;
    @Mock RecipeService recipeService;
    @Mock com.jdc.recipe_service.dev.service.recipe.ingredient.DevRecipeIngredientPersistService devIngredientPersist;
    @Mock DailyQuotaService dailyQuotaService;
    @Mock SurveyService surveyService;
    @Mock ActionImageService actionImageService;
    @Mock AsyncImageService asyncImageService;
    @Mock IngredientFocusPromptBuilder ingredientBuilder;
    @Mock CostEffectivePromptBuilder costBuilder;
    @Mock NutritionPromptBuilder nutritionBuilder;
    @Mock FineDiningPromptBuilder fineDiningBuilder;
    @Mock TransactionTemplate transactionTemplate;

    @InjectMocks DevAiRecipeFacade facade;

    private static final Long USER_ID = 1L;
    private static final String CLIENT_KEY = "key-abc";
    // 저장되는 키: dev-ai:{userId}:{sha256(clientKey)} — facade 내부 알고리즘과 일치해야 함
    private static final String NAMESPACED_KEY = "dev-ai:" + USER_ID + ":" + sha256Hex(CLIENT_KEY);
    private static final String VALID_MODEL = "gemini-2.5-flash-image";

    private static String sha256Hex(String input) {
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-256");
            return HexFormat.of().formatHex(md.digest(input.getBytes(StandardCharsets.UTF_8)));
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    private RecipeWithImageUploadRequest validRequest;

    @BeforeEach
    void setUp() {
        AiRecipeRequestDto aiReq = new AiRecipeRequestDto();
        validRequest = RecipeWithImageUploadRequest.builder()
                .aiRequest(aiReq)
                .build();
    }

    // --- createAiGenerationJob: idempotency + quota ---

    @Test
    @DisplayName("새 idempotencyKey: 쿼터 차감 + 새 job 저장 + created=true, usedToken=false 반영")
    void newKey_consumesQuotaAndCreatesJob() {
        given(jobRepository.findByIdempotencyKey(NAMESPACED_KEY)).willReturn(Optional.empty());
        given(dailyQuotaService.consumeForUserOrThrow(USER_ID, QuotaType.AI_GENERATION)).willReturn(false);
        given(jobRepository.saveAndFlush(any(RecipeGenerationJob.class))).willAnswer(inv -> {
            RecipeGenerationJob j = inv.getArgument(0);
            ReflectionTestUtils.setField(j, "id", 100L);
            return j;
        });

        JobCreateResult result = facade.createAiGenerationJob(
                validRequest, AiRecipeConcept.INGREDIENT_FOCUS, VALID_MODEL, USER_ID, CLIENT_KEY);

        assertThat(result.created()).isTrue();
        assertThat(result.usedToken()).isFalse();
        assertThat(result.jobId()).isEqualTo(100L);
        verify(dailyQuotaService).consumeForUserOrThrow(USER_ID, QuotaType.AI_GENERATION);
    }

    @Test
    @DisplayName("token으로 fallback 됐을 때 usedToken=true 그대로 반환")
    void newKey_consumeReturnsTrue_usedTokenTrue() {
        given(jobRepository.findByIdempotencyKey(NAMESPACED_KEY)).willReturn(Optional.empty());
        given(dailyQuotaService.consumeForUserOrThrow(USER_ID, QuotaType.AI_GENERATION)).willReturn(true);
        given(jobRepository.saveAndFlush(any(RecipeGenerationJob.class))).willAnswer(inv -> inv.getArgument(0));

        JobCreateResult result = facade.createAiGenerationJob(
                validRequest, AiRecipeConcept.INGREDIENT_FOCUS, VALID_MODEL, USER_ID, CLIENT_KEY);

        assertThat(result.usedToken()).isTrue();
    }

    @Test
    @DisplayName("기존 idempotencyKey: 쿼터 재차감 안 하고 created=false (controller async 재실행 차단)")
    void reusedKey_noConsumeNoSave_createdFalse() {
        RecipeGenerationJob existing = RecipeGenerationJob.builder()
                .userId(USER_ID).idempotencyKey(NAMESPACED_KEY).build();
        ReflectionTestUtils.setField(existing, "id", 50L);
        given(jobRepository.findByIdempotencyKey(NAMESPACED_KEY)).willReturn(Optional.of(existing));

        JobCreateResult result = facade.createAiGenerationJob(
                validRequest, AiRecipeConcept.INGREDIENT_FOCUS, VALID_MODEL, USER_ID, CLIENT_KEY);

        assertThat(result.created()).isFalse();
        assertThat(result.jobId()).isEqualTo(50L);
        verify(dailyQuotaService, never()).consumeForUserOrThrow(any(), any());
        verify(jobRepository, never()).saveAndFlush(any());
    }

    // --- namespacing (cross-user 충돌 방지) ---

    @Test
    @DisplayName("저장되는 idempotencyKey는 'dev-ai:{userId}:{sha256(rawKey)}'로 namespacing되어 prod/cross-user 충돌 방지 + 64자 고정")
    void newJob_storedKeyIsNamespaced() {
        given(jobRepository.findByIdempotencyKey(NAMESPACED_KEY)).willReturn(Optional.empty());
        given(dailyQuotaService.consumeForUserOrThrow(USER_ID, QuotaType.AI_GENERATION)).willReturn(false);
        given(jobRepository.saveAndFlush(any(RecipeGenerationJob.class))).willAnswer(inv -> inv.getArgument(0));

        facade.createAiGenerationJob(
                validRequest, AiRecipeConcept.INGREDIENT_FOCUS, VALID_MODEL, USER_ID, CLIENT_KEY);

        // 조회: namespaced 키로 lookup
        verify(jobRepository).findByIdempotencyKey(NAMESPACED_KEY);
        // 저장: namespaced 키로 persist
        ArgumentCaptor<RecipeGenerationJob> captor = ArgumentCaptor.forClass(RecipeGenerationJob.class);
        verify(jobRepository).saveAndFlush(captor.capture());
        String stored = captor.getValue().getIdempotencyKey();
        assertThat(stored).isEqualTo(NAMESPACED_KEY);
        // 형식 추가 검증: prod 공간과 분리되도록 dev-ai: prefix + userId + 64자 hex
        assertThat(stored).startsWith("dev-ai:" + USER_ID + ":");
        assertThat(stored.substring(("dev-ai:" + USER_ID + ":").length())).hasSize(64);
    }

    @Test
    @DisplayName("RecipeGenerationJob에 imageGenerationModel 컬럼이 저장된다")
    void newJob_imageGenModelPersisted() {
        given(jobRepository.findByIdempotencyKey(NAMESPACED_KEY)).willReturn(Optional.empty());
        given(dailyQuotaService.consumeForUserOrThrow(USER_ID, QuotaType.AI_GENERATION)).willReturn(false);
        given(jobRepository.saveAndFlush(any(RecipeGenerationJob.class))).willAnswer(inv -> inv.getArgument(0));

        facade.createAiGenerationJob(
                validRequest, AiRecipeConcept.INGREDIENT_FOCUS, "gpt-image-2-low", USER_ID, CLIENT_KEY);

        ArgumentCaptor<RecipeGenerationJob> captor = ArgumentCaptor.forClass(RecipeGenerationJob.class);
        verify(jobRepository).saveAndFlush(captor.capture());
        assertThat(captor.getValue().getImageGenerationModel()).isEqualTo("gpt-image-2-low");
    }

    // --- input validation ---

    @Test
    @DisplayName("imageGenModel이 화이트리스트에 없으면 UNSUPPORTED_IMAGE_MODEL — 쿼터 차감 전 차단")
    void invalidImageModel_throwsUnsupported_noQuotaNoSave() {
        assertThatThrownBy(() -> facade.createAiGenerationJob(
                validRequest, AiRecipeConcept.INGREDIENT_FOCUS, "dall-e-3", USER_ID, CLIENT_KEY))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNSUPPORTED_IMAGE_MODEL);

        verify(dailyQuotaService, never()).consumeForUserOrThrow(any(), any());
        verify(jobRepository, never()).saveAndFlush(any());
    }

    @Test
    @DisplayName("aiRequest가 null이면 INVALID_INPUT_VALUE")
    void nullAiRequest_throws() {
        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder()
                .aiRequest(null)
                .build();

        assertThatThrownBy(() -> facade.createAiGenerationJob(
                req, AiRecipeConcept.INGREDIENT_FOCUS, VALID_MODEL, USER_ID, CLIENT_KEY))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(dailyQuotaService, never()).consumeForUserOrThrow(any(), any());
    }

    // --- async refund accuracy (MUST FIX) ---

    @Test
    @DisplayName("async 실패 + usedToken=true → refund(..., true)로 정확히 환불")
    void asyncFailure_refundWithUsedTokenTrue() {
        RecipeGenerationJob job = mockJob(200L);
        given(jobRepository.findById(200L)).willReturn(Optional.of(job));
        // surveyService에서 throw → catch 블록 진입
        given(surveyService.getSurvey(USER_ID)).willThrow(new RuntimeException("survey down"));

        facade.processAiGenerationAsync(200L, validRequest, AiRecipeConcept.INGREDIENT_FOCUS,
                VALID_MODEL, USER_ID, /* usedToken= */ true);

        verify(dailyQuotaService).refund(USER_ID, QuotaType.AI_GENERATION, true);
    }

    @Test
    @DisplayName("async 실패 + usedToken=false → refund(..., false)로 정확히 환불 (V2의 항상 true 버그 회피)")
    void asyncFailure_refundWithUsedTokenFalse() {
        RecipeGenerationJob job = mockJob(201L);
        given(jobRepository.findById(201L)).willReturn(Optional.of(job));
        given(surveyService.getSurvey(USER_ID)).willThrow(new RuntimeException("survey down"));

        facade.processAiGenerationAsync(201L, validRequest, AiRecipeConcept.INGREDIENT_FOCUS,
                VALID_MODEL, USER_ID, /* usedToken= */ false);

        verify(dailyQuotaService).refund(USER_ID, QuotaType.AI_GENERATION, false);
        verify(dailyQuotaService, never()).refund(any(), any(), eq(true));
    }

    // --- 1.4 wiring 회귀 차단 ---

    @Test
    @DisplayName("**MUST 회귀 차단**: AI happy path — 운영 호출에 ingredients=[]/step.ingredients=[] 전달 + persistAllSystemSourced + linkStepIngredients 순서 + 같은 트랜잭션")
    void asyncSuccess_emptyThenSave_singleTransaction_correctWiring() {
        Long jobId = 300L;
        Long recipeId = 999L;
        RecipeGenerationJob job = mockJob(jobId);
        given(jobRepository.findById(jobId)).willReturn(Optional.of(job));
        // null survey → applySurveyInfoToAiRequest 즉시 return (NPE 회피, 본 테스트는 wiring만 확인)
        given(surveyService.getSurvey(USER_ID)).willReturn(null);
        given(ingredientBuilder.buildPrompt(any())).willReturn("system-prompt");

        // Generated DTO with raw 단위 (1.4 보존 정책 시뮬레이션)
        RecipeIngredientRequestDto rawIng = RecipeIngredientRequestDto.builder()
                .name("마늘").quantity("3").customUnit("쪽").build();
        RecipeStepIngredientRequestDto rawStepIng = RecipeStepIngredientRequestDto.builder()
                .name("마늘").quantity("3").customUnit("쪽").build();
        RecipeStepRequestDto rawStep = RecipeStepRequestDto.builder()
                .stepNumber(0).instruction("끓인다").action("끓이기")
                .ingredients(new ArrayList<>(List.of(rawStepIng))).build();

        RecipeCreateRequestDto generatedDto = new RecipeCreateRequestDto();
        generatedDto.setTitle("AI 레시피");
        generatedDto.setIngredients(new ArrayList<>(List.of(rawIng)));
        generatedDto.setSteps(new ArrayList<>(List.of(rawStep)));
        generatedDto.setMarketPrice(8500);

        given(grokClientService.generateRecipeJson(any(), any()))
                .willReturn(CompletableFuture.completedFuture(generatedDto));

        // TransactionTemplate.execute → invoke callback (Long return type)
        given(transactionTemplate.execute(any())).willAnswer(inv -> {
            TransactionCallback<Long> cb = inv.getArgument(0);
            return cb.doInTransaction(null);
        });

        // operational save returns recipeId
        PresignedUrlResponse savedResponse = mock(PresignedUrlResponse.class);
        given(savedResponse.getRecipeId()).willReturn(recipeId);
        given(recipeService.createRecipeAndGenerateUrls(
                any(RecipeWithImageUploadRequest.class), eq(USER_ID), eq(RecipeSourceType.AI), any()))
                .willReturn(savedResponse);

        Recipe recipeEntity = mock(Recipe.class);
        given(recipeRepository.findById(recipeId)).willReturn(Optional.of(recipeEntity));

        facade.processAiGenerationAsync(jobId, validRequest, AiRecipeConcept.INGREDIENT_FOCUS,
                VALID_MODEL, USER_ID, /* usedToken= */ true);

        // (1) 운영 호출에는 ingredients=[]/step.ingredients=[] 전달
        ArgumentCaptor<RecipeWithImageUploadRequest> reqCaptor =
                ArgumentCaptor.forClass(RecipeWithImageUploadRequest.class);
        verify(recipeService).createRecipeAndGenerateUrls(
                reqCaptor.capture(), eq(USER_ID), eq(RecipeSourceType.AI), any());
        RecipeCreateRequestDto operationalDto = reqCaptor.getValue().getRecipe();
        assertThat(operationalDto.getIngredients())
                .as("운영 호출에는 ingredients=[] 전달")
                .isEmpty();
        assertThat(operationalDto.getSteps()).hasSize(1);
        assertThat(operationalDto.getSteps().get(0).getIngredients())
                .as("step.ingredients도 비어 운영 step ingredient linker가 INGREDIENT_NOT_FOUND를 안 던지게")
                .isEmpty();

        // (2) 호출 순서: createRecipeAndGenerateUrls → persistAllSystemSourced → linkStepIngredients
        InOrder order = inOrder(recipeService, devIngredientPersist);
        order.verify(recipeService).createRecipeAndGenerateUrls(any(), eq(USER_ID), eq(RecipeSourceType.AI), any());
        order.verify(devIngredientPersist)
                .persistAllSystemSourced(eq(recipeEntity), any(), eq(8500));
        order.verify(devIngredientPersist)
                .linkStepIngredients(eq(recipeEntity), any());

        // (3) dev persist는 원본 ingredients/steps 받음 (운영 호출의 빈 리스트 아님)
        ArgumentCaptor<List<RecipeIngredientRequestDto>> ingCaptor = ArgumentCaptor.forClass(List.class);
        verify(devIngredientPersist).persistAllSystemSourced(eq(recipeEntity), ingCaptor.capture(), eq(8500));
        assertThat(ingCaptor.getValue()).containsExactly(rawIng);

        ArgumentCaptor<List<RecipeStepRequestDto>> stepCaptor = ArgumentCaptor.forClass(List.class);
        verify(devIngredientPersist).linkStepIngredients(eq(recipeEntity), stepCaptor.capture());
        assertThat(stepCaptor.getValue()).containsExactly(rawStep);

        // (4) 트랜잭션 경계: transactionTemplate.execute로 단일 tx에 묶임 (createRecipe REQUIRED join)
        verify(transactionTemplate).execute(any());

        // (5) 원본 dto는 mutate되지 않음
        assertThat(generatedDto.getIngredients())
                .as("원본 generatedDto의 ingredients는 보존 (clone에만 빈 리스트)")
                .containsExactly(rawIng);
        assertThat(generatedDto.getSteps().get(0).getIngredients())
                .as("원본 step의 ingredients도 보존")
                .containsExactly(rawStepIng);
    }

    @Test
    @DisplayName("이미지 생성 실패: PENDING recipe를 FAILED/default image로 확정한 뒤 job COMPLETED")
    @SuppressWarnings("unchecked")
    void asyncImageFailure_marksRecipeFailedBeforeJobCompleted() {
        Long jobId = 301L;
        Long recipeId = 1000L;
        RecipeGenerationJob job = mockJob(jobId);
        given(jobRepository.findById(jobId)).willReturn(Optional.of(job));
        given(surveyService.getSurvey(USER_ID)).willReturn(null);
        given(ingredientBuilder.buildPrompt(any())).willReturn("system-prompt");

        RecipeIngredientRequestDto rawIng = RecipeIngredientRequestDto.builder()
                .name("양파").quantity("1").customUnit("개").build();

        RecipeCreateRequestDto generatedDto = new RecipeCreateRequestDto();
        generatedDto.setTitle("AI recipe");
        generatedDto.setIngredients(new ArrayList<>(List.of(rawIng)));
        generatedDto.setSteps(new ArrayList<>());

        given(grokClientService.generateRecipeJson(any(), any()))
                .willReturn(CompletableFuture.completedFuture(generatedDto));

        given(transactionTemplate.execute(any())).willAnswer(inv -> {
            TransactionCallback<Long> cb = inv.getArgument(0);
            return cb.doInTransaction(null);
        });
        org.mockito.Mockito.doAnswer(inv -> {
            Consumer<TransactionStatus> cb = inv.getArgument(0);
            cb.accept(null);
            return null;
        }).when(transactionTemplate).executeWithoutResult(any());

        PresignedUrlResponse savedResponse = mock(PresignedUrlResponse.class);
        given(savedResponse.getRecipeId()).willReturn(recipeId);
        given(recipeService.createRecipeAndGenerateUrls(
                any(RecipeWithImageUploadRequest.class), eq(USER_ID), eq(RecipeSourceType.AI), any()))
                .willReturn(savedResponse);

        Recipe recipeEntity = mock(Recipe.class);
        given(recipeEntity.getImageStatus()).willReturn(RecipeImageStatus.PENDING);
        given(recipeRepository.findById(recipeId)).willReturn(Optional.of(recipeEntity));
        given(asyncImageService.generateAndUploadAiImage(recipeId, true, VALID_MODEL))
                .willThrow(new RuntimeException("image down"));

        facade.processAiGenerationAsync(jobId, validRequest, AiRecipeConcept.INGREDIENT_FOCUS,
                VALID_MODEL, USER_ID, /* usedToken= */ true);

        verify(recipeEntity).updateImageKey(AsyncImageService.DEFAULT_IMAGE_KEY);
        verify(recipeEntity).updateImageStatus(RecipeImageStatus.FAILED);
        verify(recipeEntity).updateImageGenerationModel(VALID_MODEL);
        verify(recipeEntity).applyVisibility(RecipeVisibility.PUBLIC);
        assertThat(job.getStatus()).isEqualTo(com.jdc.recipe_service.domain.type.JobStatus.COMPLETED);
        assertThat(job.getResultRecipeId()).isEqualTo(recipeId);
    }

    private RecipeGenerationJob mockJob(Long id) {
        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .userId(USER_ID)
                .idempotencyKey(NAMESPACED_KEY)
                .build();
        ReflectionTestUtils.setField(job, "id", id);
        return job;
    }
}
