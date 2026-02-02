package com.jdc.recipe_service.facade;

import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.entity.RecipeGenerationJob;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.*;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
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
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;


@Service
@RequiredArgsConstructor
@Slf4j
public class AiRecipeFacade {

    private final RecipeRepository recipeRepository;
    private final RecipeGenerationJobRepository jobRepository;

    private final GrokClientService grokClientService;
    private final GeminiClientService geminiClientService;
    private final RecipeService recipeService;
    private final DailyQuotaService dailyQuotaService;
    private final SurveyService surveyService;
    private final ActionImageService actionImageService;
    private final UnitService unitService;
    private final AsyncImageService asyncImageService;
    private final DeferredResultHolder deferredResultHolder;

    private final IngredientFocusPromptBuilder ingredientBuilder;
    private final CostEffectivePromptBuilder costBuilder;
    private final NutritionPromptBuilder nutritionBuilder;
    private final FineDiningPromptBuilder fineDiningBuilder;


    /**
     * 트랜잭션 없이 AI 호출 수행 후, 저장 시점에만 트랜잭션 참여
     */
    public DeferredResult<ResponseEntity<PresignedUrlResponse>> generateAndSave(
            RecipeWithImageUploadRequest request,
            AiRecipeConcept concept,
            Long userId
    ) {

        if (request.getAiRequest() == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "AI 요청 정보가 없습니다.");
        }

        boolean usedToken = dailyQuotaService.consumeForUserOrThrow(userId, QuotaType.AI_GENERATION);

        try {
            AiRecipeRequestDto aiReq = request.getAiRequest();
            aiReq.setUserId(userId);

            UserSurveyDto survey = surveyService.getSurvey(userId);
            applySurveyInfoToAiRequest(aiReq, survey);

            RecipeCreateRequestDto generatedDto;

            switch (concept) {
                case INGREDIENT_FOCUS -> {
                    String systemPrompt = ingredientBuilder.buildPrompt(aiReq);
                    String userTrigger = "위 정보를 바탕으로 레시피 JSON을 생성해줘.";
                    generatedDto = grokClientService.generateRecipeJson(systemPrompt, userTrigger).join();
                }
                case COST_EFFECTIVE -> {
                    String systemPrompt = costBuilder.buildPrompt(aiReq);
                    String userTrigger = "위 조건에 맞춰 JSON 결과만 출력해.";
                    generatedDto = grokClientService.generateRecipeJson(systemPrompt, userTrigger).join();
                }
                case NUTRITION_BALANCE -> {
                    generatedDto = processNutritionLogic(aiReq);
                }
                case FINE_DINING -> {
                    FineDiningPromptBuilder.FineDiningPrompt promptResult = fineDiningBuilder.buildPrompt(aiReq);
                    generatedDto = geminiClientService.generateRecipeJson(
                            promptResult.getSystemInstruction(),
                            promptResult.getUserMessage()
                    ).join();
                }
                default -> throw new IllegalArgumentException("Unknown Concept");
            }

            generatedDto.setIngredients(correctIngredientUnits(generatedDto.getIngredients()));

            for (RecipeStepRequestDto step : generatedDto.getSteps()) {
                if (step.getAction() != null) {
                    String key = actionImageService.generateImageKey(concept, step.getAction());
                    step.updateImageKey(key);
                }
            }

            RecipeWithImageUploadRequest processingRequest = RecipeWithImageUploadRequest.builder()
                    .aiRequest(aiReq)
                    .recipe(generatedDto)
                    .files(request.getFiles())
                    .build();

            PresignedUrlResponse savedResponse = recipeService.createRecipeAndGenerateUrls(
                    processingRequest, userId, RecipeSourceType.AI, concept
            );
            Long recipeId = savedResponse.getRecipeId();
            DeferredResult<ResponseEntity<PresignedUrlResponse>> result = deferredResultHolder.create(recipeId, 60000L);

            CompletableFuture.runAsync(() -> {
                try {
                    asyncImageService.generateAndUploadAiImage(recipeId,true);
                } catch (Exception e) {
                    log.error("비동기 작업 중 예상치 못한 오류 발생 ID: {}", recipeId, e);
                }
            });

            return result;

        } catch (Exception e) {
            dailyQuotaService.refund(userId, QuotaType.AI_GENERATION, usedToken);
            throw e;
        }
    }

    /**
     * [Phase 1] V2 작업 접수
     * - Idempotency Key를 확인하여 중복 작업을 방지하고 Job ID를 반환합니다.
     */
    @Transactional
    public Long createAiGenerationJobV2(RecipeWithImageUploadRequest request, AiRecipeConcept concept, Long userId, String idempotencyKey) {
        if (request.getAiRequest() == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "AI 요청 정보가 없습니다.");
        }

        Optional<RecipeGenerationJob> existingJob = jobRepository.findByIdempotencyKey(idempotencyKey);
        if (existingJob.isPresent()) {
            RecipeGenerationJob job = existingJob.get();
            log.info("♻️ [V2] 기존 작업 재사용 - Key: {}, JobID: {}", idempotencyKey, job.getId());
            return job.getId();
        }

        dailyQuotaService.consumeForUserOrThrow(userId, QuotaType.AI_GENERATION);

        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .userId(userId)
                .jobType(JobType.AI_GENERATION)
                .status(JobStatus.PENDING)
                .progress(0)
                .idempotencyKey(idempotencyKey)
                .build();

        jobRepository.save(job);
        log.info("🆕 [V2] 신규 작업 생성 - JobID: {}", job.getId());

        return job.getId();
    }

    /**
     * [Phase 2] V2 비동기 처리 (백그라운드)
     * - 서버가 끝까지 책임지고 수행합니다.
     */
    @Async("recipeExtractionExecutor")
    public void processAiGenerationAsyncV2(Long jobId, RecipeWithImageUploadRequest request, AiRecipeConcept concept, Long userId) {
        long startTime = System.currentTimeMillis();
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        if (job.getStatus() == JobStatus.COMPLETED) return;

        try {
            updateProgress(job, JobStatus.IN_PROGRESS, 0);

            AiRecipeRequestDto aiReq = request.getAiRequest();
            aiReq.setUserId(userId);
            UserSurveyDto survey = surveyService.getSurvey(userId);
            applySurveyInfoToAiRequest(aiReq, survey);

            long textGenStart = System.currentTimeMillis();
            updateProgress(job, JobStatus.IN_PROGRESS, 10);

            RecipeCreateRequestDto generatedDto;

            switch (concept) {
                case INGREDIENT_FOCUS -> {
                    String systemPrompt = ingredientBuilder.buildPrompt(aiReq);
                    String userTrigger = "위 정보를 바탕으로 레시피 JSON을 생성해줘.";
                    generatedDto = grokClientService.generateRecipeJson(systemPrompt, userTrigger).join();
                }
                case COST_EFFECTIVE -> {
                    String systemPrompt = costBuilder.buildPrompt(aiReq);
                    String userTrigger = "위 조건에 맞춰 JSON 결과만 출력해.";
                    generatedDto = grokClientService.generateRecipeJson(systemPrompt, userTrigger).join();
                }
                case NUTRITION_BALANCE -> {
                    generatedDto = processNutritionLogic(aiReq);
                }
                case FINE_DINING -> {
                    FineDiningPromptBuilder.FineDiningPrompt promptResult = fineDiningBuilder.buildPrompt(aiReq);
                    generatedDto = geminiClientService.generateRecipeJson(
                            promptResult.getSystemInstruction(),
                            promptResult.getUserMessage()
                    ).join();
                }
                default -> throw new IllegalArgumentException("Unknown Concept");
            }

            long textGenEnd = System.currentTimeMillis();
            log.info("⏱️ [Performance] AI 텍스트 생성 소요 시간: {}ms", (textGenEnd - textGenStart));

            long dbSaveStart = System.currentTimeMillis();

            generatedDto.setIngredients(correctIngredientUnits(generatedDto.getIngredients()));

            for (RecipeStepRequestDto step : generatedDto.getSteps()) {
                if (step.getAction() != null) {
                    String key = actionImageService.generateImageKey(concept, step.getAction());
                    step.updateImageKey(key);
                }
            }

            RecipeWithImageUploadRequest processingRequest = RecipeWithImageUploadRequest.builder()
                    .aiRequest(aiReq)
                    .recipe(generatedDto)
                    .files(request.getFiles())
                    .build();

            PresignedUrlResponse savedResponse = recipeService.createRecipeAndGenerateUrls(
                    processingRequest, userId, RecipeSourceType.AI, concept
            );

            Long recipeId = savedResponse.getRecipeId();

            long dbSaveEnd = System.currentTimeMillis();
            log.info("⏱️ [Performance] DB 저장 및 가공 소요 시간: {}ms", (dbSaveEnd - dbSaveStart));

            long imageGenStart = System.currentTimeMillis();
            updateProgress(job, JobStatus.IN_PROGRESS, 75);

            try {
                log.info("🎨 이미지 생성 시작 (동기 실행)");
                asyncImageService.generateAndUploadAiImage(recipeId, true);
                log.info("✅ 이미지 생성 완료");
            } catch (Exception e) {
                log.warn("⚠️ 이미지 생성 중 오류 발생 (레시피는 유지): {}", e.getMessage());
            }

            long imageGenEnd = System.currentTimeMillis();
            log.info("⏱️ [Performance] 이미지 생성 소요 시간: {}ms", (imageGenEnd - imageGenStart));

            job.setResultRecipeId(recipeId);

            updateProgress(job, JobStatus.COMPLETED, 100);
            log.info("✅ [Performance] 전체 작업 총 소요 시간: {}ms", (System.currentTimeMillis() - startTime));

        } catch (Exception e) {
            log.error("V2 생성 실패 JobID: {}", jobId, e);
            job.setErrorMessage(e.getMessage());
            updateProgress(job, JobStatus.FAILED, 0);

            dailyQuotaService.refund(userId, QuotaType.AI_GENERATION, true);
        }
    }

    /**
     * [Phase 3] V2 상태 조회 (Polling)
     */
    @Transactional(readOnly = true)
    public JobStatusDto getJobStatus(Long jobId) {
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        Long recipeId = job.getResultRecipeId();

        if (job.getStatus() == JobStatus.COMPLETED && recipeId != null) {
            if (!recipeRepository.existsById(recipeId)) {
                recipeId = null;
            }
        }

        return JobStatusDto.builder()
                .jobId(job.getId())
                .status(job.getStatus())
                .resultRecipeId(recipeId)
                .errorMessage(job.getErrorMessage())
                .progress(job.getProgress())
                .build();
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void updateProgress(RecipeGenerationJob job, JobStatus status, int progress) {
        job.updateProgress(status, progress);
        jobRepository.saveAndFlush(job);
    }

    private RecipeCreateRequestDto processNutritionLogic(AiRecipeRequestDto aiReq) {
        String step1System = nutritionBuilder.buildStep1Prompt(aiReq);
        String step1Trigger = "위 조건에 맞춰 JSON 결과만 출력해.";
        String step1Json = grokClientService.generateRaw(step1System, step1Trigger).join();

        String step2System = nutritionBuilder.buildStep2Prompt(step1Json);
        String step2Trigger = "위 재료와 양념을 조합하여 완벽한 레시피를 JSON으로 만들어줘.";
        return grokClientService.generateRecipeJson(step2System, step2Trigger).join();
    }

    private void applySurveyInfoToAiRequest(AiRecipeRequestDto aiReq, UserSurveyDto survey) {
        if (survey == null) return;
        Optional.ofNullable(survey.getSpiceLevel()).ifPresent(aiReq::setSpiceLevel);
        aiReq.setAllergy(survey.getAllergy());
        if (CollectionUtils.isEmpty(aiReq.getTags())) {
            aiReq.setTags(new ArrayList<>(survey.getTags()));
        }
    }

    private List<RecipeIngredientRequestDto> correctIngredientUnits(List<RecipeIngredientRequestDto> ingredients) {
        if (ingredients == null) {
            return new ArrayList<>();
        }
        return ingredients.stream()
                .map(ing -> {
                    String finalUnit = unitService.getDefaultUnit(ing.getName())
                            .orElse(ing.getCustomUnit());
                    return RecipeIngredientRequestDto.builder()
                            .name(ing.getName())
                            .quantity(ing.getQuantity())
                            .customPrice(ing.getCustomPrice())
                            .customUnit(finalUnit)
                            .customCalories(ing.getCustomCalories())
                            .customCarbohydrate(ing.getCustomCarbohydrate())
                            .customProtein(ing.getCustomProtein())
                            .customFat(ing.getCustomFat())
                            .customSugar(ing.getCustomSugar())
                            .customSodium(ing.getCustomSodium())
                            .build();
                })
                .collect(Collectors.toList());
    }
}