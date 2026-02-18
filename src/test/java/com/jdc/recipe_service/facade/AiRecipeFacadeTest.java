package com.jdc.recipe_service.facade;

import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.*;
import com.jdc.recipe_service.service.DailyQuotaService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.SurveyService;
import com.jdc.recipe_service.service.ai.GeminiClientService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import com.jdc.recipe_service.util.ActionImageService;
import com.jdc.recipe_service.util.DeferredResultHolder;
import com.jdc.recipe_service.util.UnitService;
import com.jdc.recipe_service.util.prompt.CostEffectivePromptBuilder;
import com.jdc.recipe_service.util.prompt.FineDiningPromptBuilder;
import com.jdc.recipe_service.util.prompt.IngredientFocusPromptBuilder;
import com.jdc.recipe_service.util.prompt.NutritionPromptBuilder;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class AiRecipeFacadeTest {

    @Mock private RecipeRepository recipeRepository;
    @Mock private RecipeGenerationJobRepository jobRepository;
    @Mock private GrokClientService grokClientService;
    @Mock private GeminiClientService geminiClientService;
    @Mock private RecipeService recipeService;
    @Mock private DailyQuotaService dailyQuotaService;
    @Mock private SurveyService surveyService;
    @Mock private ActionImageService actionImageService;
    @Mock private UnitService unitService;
    @Mock private AsyncImageService asyncImageService;
    @Mock private DeferredResultHolder deferredResultHolder;
    @Mock private IngredientFocusPromptBuilder ingredientBuilder;
    @Mock private CostEffectivePromptBuilder costBuilder;
    @Mock private NutritionPromptBuilder nutritionBuilder;
    @Mock private FineDiningPromptBuilder fineDiningBuilder;

    private AiRecipeFacade aiRecipeFacade;

    @BeforeEach
    void setUp() {
        aiRecipeFacade = new AiRecipeFacade(
                recipeRepository, jobRepository, grokClientService, geminiClientService,
                recipeService, dailyQuotaService, surveyService, actionImageService,
                unitService, asyncImageService, deferredResultHolder,
                ingredientBuilder, costBuilder, nutritionBuilder, fineDiningBuilder
        );
    }

    @Test
    @DisplayName("[V2] AI 레시피 생성 작업 접수 성공 테스트")
    void createAiGenerationJobV2_Success() {
        // Given
        RecipeWithImageUploadRequest request = new RecipeWithImageUploadRequest();
        request.setAiRequest(new AiRecipeRequestDto());
        String idempotencyKey = "ai-test-key";
        Long userId = 1L;
        Long expectedJobId = 123L;

        when(jobRepository.findByIdempotencyKey(anyString())).thenReturn(Optional.empty());

        // save 호출 시 전달받은 객체에 ID를 세팅하여 반환 (JobID null 문제 완벽 해결)
        when(jobRepository.save(any(RecipeGenerationJob.class))).thenAnswer(invocation -> {
            RecipeGenerationJob job = invocation.getArgument(0);
            java.lang.reflect.Field idField = job.getClass().getDeclaredField("id");
            idField.setAccessible(true);
            idField.set(job, expectedJobId);
            return job;
        });

        // When
        Long jobId = aiRecipeFacade.createAiGenerationJobV2(request, AiRecipeConcept.INGREDIENT_FOCUS, userId, idempotencyKey);

        // Then
        assertNotNull(jobId, "반환된 JobID는 null일 수 없습니다.");
        assertEquals(expectedJobId, jobId);
        verify(dailyQuotaService).consumeForUserOrThrow(eq(userId), eq(QuotaType.AI_GENERATION));
    }

    @Test
    @DisplayName("[V2] AI 비동기 로직 - 성공 테스트")
    void processAiGenerationAsyncV2_Success() throws Exception {
        // Given
        Long jobId = 1L;
        Long userId = 1L;
        Long recipeId = 500L;

        RecipeWithImageUploadRequest request = new RecipeWithImageUploadRequest();
        AiRecipeRequestDto aiReq = new AiRecipeRequestDto();
        aiReq.setTags(new ArrayList<>());
        request.setAiRequest(aiReq);

        // Job 객체 생성 및 ID 강제 주입
        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .userId(userId).status(JobStatus.PENDING).build();
        java.lang.reflect.Field idField = job.getClass().getDeclaredField("id");
        idField.setAccessible(true);
        idField.set(job, jobId);

        // NPE 방지를 위한 Survey 데이터 풀 세팅
        UserSurveyDto mockSurvey = new UserSurveyDto();
        mockSurvey.setTags(new HashSet<>());
        mockSurvey.setAllergy("");
        mockSurvey.setSpiceLevel(1);

        RecipeCreateRequestDto mockResult = new RecipeCreateRequestDto();
        mockResult.setIngredients(new ArrayList<>());
        mockResult.setSteps(new ArrayList<>());

        when(jobRepository.findById(eq(jobId))).thenReturn(Optional.of(job));
        when(surveyService.getSurvey(anyLong())).thenReturn(mockSurvey);
        when(ingredientBuilder.buildPrompt(any())).thenReturn("Mock Prompt");
        when(grokClientService.generateRecipeJson(any(), any())).thenReturn(CompletableFuture.completedFuture(mockResult));

        when(recipeService.createRecipeAndGenerateUrls(any(), anyLong(), any(), any()))
                .thenReturn(PresignedUrlResponse.builder().recipeId(recipeId).build());

        // When
        aiRecipeFacade.processAiGenerationAsyncV2(jobId, request, AiRecipeConcept.INGREDIENT_FOCUS, userId);

        // Then
        assertEquals(JobStatus.COMPLETED, job.getStatus());
        assertEquals(100, job.getProgress());
        assertEquals(recipeId, job.getResultRecipeId());
    }

    @Test
    @DisplayName("[V2] AI 비동기 로직 - 실패 시 환불 테스트")
    void processAiGenerationAsyncV2_Failure() throws Exception {
        // Given
        Long jobId = 1L;
        Long userId = 1L;

        RecipeWithImageUploadRequest request = new RecipeWithImageUploadRequest();
        request.setAiRequest(new AiRecipeRequestDto());

        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .userId(userId).status(JobStatus.PENDING).build();
        java.lang.reflect.Field idField = job.getClass().getDeclaredField("id");
        idField.setAccessible(true);
        idField.set(job, jobId);

        when(jobRepository.findById(eq(jobId))).thenReturn(Optional.of(job));
        when(surveyService.getSurvey(anyLong())).thenThrow(new RuntimeException("AI Crash"));

        // When
        aiRecipeFacade.processAiGenerationAsyncV2(jobId, request, AiRecipeConcept.INGREDIENT_FOCUS, userId);

        // Then
        assertEquals(JobStatus.FAILED, job.getStatus());
        verify(dailyQuotaService).refund(eq(userId), eq(QuotaType.AI_GENERATION), eq(true));
    }
}