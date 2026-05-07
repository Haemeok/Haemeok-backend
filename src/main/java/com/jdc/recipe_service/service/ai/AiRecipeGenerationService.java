package com.jdc.recipe_service.service.ai;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.JobStatusDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.recipe.RecipeGenerationJob;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.entity.credit.CreditCostEntity;
import com.jdc.recipe_service.domain.entity.recipe.RecipeAccess;
import com.jdc.recipe_service.domain.repository.RecipeGenerationJobRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.repository.credit.CreditCostRepository;
import com.jdc.recipe_service.domain.repository.recipe.RecipeAccessRepository;
import com.jdc.recipe_service.domain.type.*;
import com.jdc.recipe_service.domain.type.credit.CreditCost;
import com.jdc.recipe_service.domain.type.recipe.*;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeActivityService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.SurveyService;
import com.jdc.recipe_service.service.image.RecipeImageMatchingService;
import com.jdc.recipe_service.service.user.UserCreditService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import com.jdc.recipe_service.util.ActionImageService;
import com.jdc.recipe_service.util.UnitService;
import com.jdc.recipe_service.util.prompt.CostEffectivePromptBuilder;
import com.jdc.recipe_service.util.prompt.FineDiningPromptBuilder;
import com.jdc.recipe_service.util.prompt.IngredientFocusPromptBuilder;
import com.jdc.recipe_service.util.prompt.NutritionPromptBuilder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class AiRecipeGenerationService {

    private static final int DEFAULT_COST_TEXT = 1;
    private static final int DEFAULT_COST_IMAGE = 3;

    private final RecipeRepository recipeRepository;
    private final RecipeGenerationJobRepository jobRepository;
    private final RecipeAccessRepository recipeAccessRepository;
    private final UserRepository userRepository;
    private final CreditCostRepository creditCostRepository;
    private final GrokClientService grokClientService;
    private final GeminiClientService geminiClientService;
    private final RecipeService recipeService;
    private final UserCreditService userCreditService;
    private final SurveyService surveyService;
    private final ActionImageService actionImageService;
    private final UnitService unitService;
    private final AsyncImageService asyncImageService;
    private final RecipeActivityService recipeActivityService;
    private final RecipeImageMatchingService recipeImageMatchingService;

    private final IngredientFocusPromptBuilder ingredientBuilder;
    private final CostEffectivePromptBuilder costBuilder;
    private final NutritionPromptBuilder nutritionBuilder;
    private final FineDiningPromptBuilder fineDiningBuilder;

    private final TransactionTemplate transactionTemplate;

    @Qualifier("recipeExtractionExecutor")
    private final Executor recipeExtractionExecutor;

    @Transactional
    public Long createAiGenerationJob(RecipeWithImageUploadRequest request, AiRecipeConcept concept, Long userId, String idempotencyKey, RecipeDisplayMode mode) {
        if (request.getAiRequest() == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "AI 요청 정보가 없습니다.");
        }

        Optional<RecipeGenerationJob> existingJob = jobRepository.findByIdempotencyKey(idempotencyKey);
        if (existingJob.isPresent()) {
            log.info("♻️ [AI V2] 기존 작업 재사용 - Key: {}, JobID: {}", idempotencyKey, existingJob.get().getId());
            return existingJob.get().getId();
        }

        RecipeGenerationJob job = RecipeGenerationJob.builder()
                .userId(userId)
                .jobType(JobType.AI_GENERATION)
                .status(JobStatus.PENDING)
                .progress(0)
                .idempotencyKey(idempotencyKey)
                .displayMode(mode)
                .build();
        jobRepository.save(job);

        CreditCost costType = (mode == RecipeDisplayMode.IMAGE_MODE)
                ? CreditCost.AI_RECIPE_IMAGE
                : CreditCost.AI_RECIPE_TEXT;

        int cost = getCostFromDb(costType);
        userCreditService.useCredit(userId, cost, costType.name(), job.getId(), idempotencyKey);

        log.info("🆕 [AI V2] 작업 생성 완료 - JobID: {}, UserID: {}, Cost: {}, Mode: {}", job.getId(), userId, cost, mode);

        return job.getId();
    }

    @Async("recipeExtractionExecutor")
    public void processAiGenerationAsync(Long jobId, RecipeWithImageUploadRequest request, AiRecipeConcept concept, Long userId, RecipeDisplayMode mode) {
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        if (job.getStatus() == JobStatus.COMPLETED) return;

        try {
            updateProgress(job, JobStatus.IN_PROGRESS, 5);
            log.info("👨‍🍳 [AI 요리사] 조리 시작 (Job: {}, Concept: {})", jobId, concept);

            Long resultRecipeId = processActualGenerationLogic(request, concept, userId, mode, job);

            completeJobInTransaction(jobId, resultRecipeId);

            registerRecipeToUser(userId, resultRecipeId);

            try {
                String nickname = userRepository.findById(userId)
                        .map(User::getNickname)
                        .orElse("Unknown Chef");

                ActivityLogType logType = ActivityLogType.fromConcept(concept);
                recipeActivityService.saveLog(userId, nickname, logType);
            } catch (Exception e) {
                log.warn("⚠️ 로그 저장 실패: {}", e.getMessage());
            }

        } catch (Exception e) {
            handleAsyncError(job, userId, e, mode);
        }
    }

    private Long processActualGenerationLogic(RecipeWithImageUploadRequest request, AiRecipeConcept concept, Long userId, RecipeDisplayMode mode, RecipeGenerationJob job) {
        long startTime = System.currentTimeMillis();

        AiRecipeRequestDto aiReq = request.getAiRequest();
        aiReq.setUserId(userId);

        UserSurveyDto survey = surveyService.getSurvey(userId);
        applySurveyInfoToAiRequest(aiReq, survey);

        updateProgress(job, JobStatus.IN_PROGRESS, 20);

        RecipeCreateRequestDto generatedDto;
        try {
            generatedDto = generateTextRecipeByConcept(concept, aiReq);
        } catch (Exception e) {
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 텍스트 생성 실패: " + e.getMessage());
        }

        updateProgress(job, JobStatus.IN_PROGRESS, 50);
        log.info("⚡ [AI 생성] 텍스트 완료. 병렬 처리 시작 (Mode: {})", mode);

        generatedDto.setIngredients(correctIngredientUnits(generatedDto.getIngredients()));

        CompletableFuture<String> mainImageTask;
        if (mode == RecipeDisplayMode.IMAGE_MODE) {
            mainImageTask = asyncImageService.generateImageFromDto(generatedDto, userId)
                    .exceptionally(ex -> {
                        log.warn("⚠️ 메인 이미지 생성 실패: {}", ex.getMessage());
                        return null;
                    });
        } else {
            mainImageTask = CompletableFuture.completedFuture(null);
        }

        for (RecipeStepRequestDto step : generatedDto.getSteps()) {
            if (step.getAction() != null) {
                String key = actionImageService.generateImageKey(concept, step.getAction());
                step.updateImageKey(key);
            }
        }

        String generatedImageUrl = mainImageTask.join();

        if (generatedImageUrl != null && !generatedImageUrl.isBlank()) {
            log.info("🎨 메인 이미지 생성 완료 -> PUBLIC / LISTED");
            generatedDto.setImageKey(extractS3Key(generatedImageUrl));
            generatedDto.setImageStatus(RecipeImageStatus.READY);

            generatedDto.setVisibility(RecipeVisibility.PUBLIC);
            generatedDto.setListingStatus(RecipeListingStatus.LISTED);
        } else {
            log.info("📝 텍스트 모드(또는 실패) -> PUBLIC / UNLISTED (link-only)");

            DishType currentDishType = DishType.fromDisplayName(generatedDto.getDishType());

            String matchedImageKey = recipeImageMatchingService.findMatchingImageKey(
                    generatedDto.getImageMatchKeywords(), currentDishType);

            if (matchedImageKey != null) {
                log.info("✨ [이미지 매칭] 기존 썸네일 획득 성공: {}", matchedImageKey);
                generatedDto.setImageKey(matchedImageKey);
            } else {
                String defaultCategoryImage = recipeImageMatchingService.getDefaultImageKeyForDishType(
                        generatedDto.getDishType());
                log.info("⚠️ [매칭 실패] 카테고리별 기본 이미지를 적용합니다: {}", defaultCategoryImage);
                generatedDto.setImageKey(defaultCategoryImage);
            }

            // TODO: 여기서 [키워드 검색 이미지 / 기본 이미지] 매칭 로직이 들어갈 자리입니다.

            generatedDto.setImageStatus(RecipeImageStatus.READY);
            // 매칭 실패로 카테고리 기본 이미지를 쓴 경우 — 검색에 노출되지는 않지만 link로는 접근 가능.
            // 정책 V1.x: PUBLIC + UNLISTED. 기존 RESTRICTED 의도("검색 미노출 공개")를 의미별 조합으로 재표현.
            generatedDto.setVisibility(RecipeVisibility.PUBLIC);
            generatedDto.setListingStatus(RecipeListingStatus.UNLISTED);
            generatedDto.setIsPrivate(false);
        }
        generatedDto.setLifecycleStatus(RecipeLifecycleStatus.ACTIVE);

        updateProgress(job, JobStatus.IN_PROGRESS, 90);

        RecipeWithImageUploadRequest processingRequest = RecipeWithImageUploadRequest.builder()
                .aiRequest(aiReq)
                .recipe(generatedDto)
                .files(request.getFiles())
                .build();

        PresignedUrlResponse savedResponse = saveRecipeTransactional(processingRequest, userId, concept);

        log.info("✅ AI 생성 전체 완료: {}ms. RecipeID: {}", (System.currentTimeMillis() - startTime), savedResponse.getRecipeId());
        return savedResponse.getRecipeId();
    }

    private void handleAsyncError(RecipeGenerationJob job, Long userId, Exception e, RecipeDisplayMode mode) {
        log.error("❌ AI 생성 실패 JobID: {} - {}", job.getId(), e.getMessage());

        ErrorCode errorCode = (e instanceof CustomException ce) ? ce.getErrorCode() : ErrorCode.INTERNAL_SERVER_ERROR;
        String clientMsg = (e instanceof CustomException) ? e.getMessage() : "AI 생성 중 오류가 발생했습니다.";

        job.setErrorMessage(errorCode.getCode() + "::" + clientMsg);
        updateProgress(job, JobStatus.FAILED, 0);

        if (errorCode != ErrorCode.INVALID_INPUT_VALUE) {
            CreditCost costType = (mode == RecipeDisplayMode.IMAGE_MODE) ? CreditCost.AI_RECIPE_IMAGE : CreditCost.AI_RECIPE_TEXT;
            int refundAmount = getCostFromDb(costType);
            log.info("💸 시스템 오류로 크레딧 환불 (UserID: {}, Amount: {})", userId, refundAmount);
            String refundReason = "시스템 오류 환불 (Job ID: " + job.getId() + ")";
            userCreditService.refundCredit(userId, refundAmount, refundReason, "RECIPE_JOB", job.getId());
        }
    }

    private RecipeCreateRequestDto generateTextRecipeByConcept(AiRecipeConcept concept, AiRecipeRequestDto aiReq) {
        switch (concept) {
            case INGREDIENT_FOCUS -> {
                String systemPrompt = ingredientBuilder.buildPrompt(aiReq);
                return grokClientService.generateRecipeJson(systemPrompt, "위 정보를 바탕으로 레시피 JSON을 생성해줘.").join();
            }
            case COST_EFFECTIVE -> {
                String systemPrompt = costBuilder.buildPrompt(aiReq);
                return grokClientService.generateRecipeJson(systemPrompt, "위 조건에 맞춰 JSON 결과만 출력해.").join();
            }
            case NUTRITION_BALANCE -> {
                return processNutritionLogic(aiReq);
            }
            case FINE_DINING -> {
                FineDiningPromptBuilder.FineDiningPrompt promptResult = fineDiningBuilder.buildPrompt(aiReq);
                return geminiClientService.generateRecipeJson(
                        promptResult.getSystemInstruction(),
                        promptResult.getUserMessage()
                ).join();
            }
            default -> throw new IllegalArgumentException("지원하지 않는 레시피 컨셉입니다.");
        }
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void updateProgress(RecipeGenerationJob job, JobStatus status, int progress) {
        job.updateProgress(status, progress);
        jobRepository.saveAndFlush(job);
    }

    private PresignedUrlResponse saveRecipeTransactional(RecipeWithImageUploadRequest request, Long userId, AiRecipeConcept concept) {
        return transactionTemplate.execute(status ->
                recipeService.createRecipeAndGenerateUrls(request, userId, RecipeSourceType.AI, concept)
        );
    }

    private void completeJobInTransaction(Long jobId, Long resultRecipeId) {
        transactionTemplate.executeWithoutResult(status -> {
            RecipeGenerationJob job = jobRepository.findById(jobId)
                    .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));
            job.setResultRecipeId(resultRecipeId);
            job.updateProgress(JobStatus.COMPLETED, 100);
            jobRepository.saveAndFlush(job);
        });
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    protected void grantRecipeOwnership(Long userId, Long recipeId, RecipeAccessRole role) {
        if (recipeAccessRepository.existsByUserIdAndRecipeId(userId, recipeId)) return;
        try {
            User user = userRepository.getReferenceById(userId);
            Recipe recipe = recipeRepository.getReferenceById(recipeId);

            recipeAccessRepository.save(RecipeAccess.builder()
                    .user(user)
                    .recipe(recipe)
                    .role(role)
                    .build());
        } catch (Exception e) {
            log.warn("⚠️ 권한 부여 실패: {}", e.getMessage());
        }
    }

    @Transactional(readOnly = true)
    public JobStatusDto getJobStatus(Long jobId) {
        RecipeGenerationJob job = jobRepository.findById(jobId)
                .orElseThrow(() -> new CustomException(ErrorCode.RESOURCE_NOT_FOUND));

        Long recipeId = job.getResultRecipeId();
        if (job.getStatus() == JobStatus.COMPLETED && recipeId != null) {
            if (!recipeRepository.existsById(recipeId)) recipeId = null;
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

    private void registerRecipeToUser(Long userId, Long recipeId) {
        if (userId == null || recipeId == null) return;

        grantRecipeOwnership(userId, recipeId, RecipeAccessRole.OWNER);
    }

    private int getCostFromDb(CreditCost costType) {
        return creditCostRepository.findByCode(costType.name())
                .map(CreditCostEntity::getCost)
                .orElseGet(() -> (costType == CreditCost.AI_RECIPE_IMAGE) ? DEFAULT_COST_IMAGE : DEFAULT_COST_TEXT);
    }

    private RecipeCreateRequestDto processNutritionLogic(AiRecipeRequestDto aiReq) {
        String step1System = nutritionBuilder.buildStep1Prompt(aiReq);
        String step1Json = grokClientService.generateRaw(step1System, "위 조건에 맞춰 JSON 결과만 출력해.").join();

        String step2System = nutritionBuilder.buildStep2Prompt(step1Json);
        return grokClientService.generateRecipeJson(step2System, "위 재료와 양념을 조합하여 완벽한 레시피를 JSON으로 만들어줘.").join();
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
        if (ingredients == null) return new ArrayList<>();
        return ingredients.stream().map(ing -> {
            String finalUnit = unitService.getDefaultUnit(ing.getName()).orElse(ing.getCustomUnit());
            return RecipeIngredientRequestDto.builder()
                    .name(ing.getName()).quantity(ing.getQuantity()).customPrice(ing.getCustomPrice())
                    .customUnit(finalUnit)
                    .customCalories(ing.getCustomCalories()).customCarbohydrate(ing.getCustomCarbohydrate())
                    .customProtein(ing.getCustomProtein()).customFat(ing.getCustomFat())
                    .customSugar(ing.getCustomSugar()).customSodium(ing.getCustomSodium())
                    .build();
        }).collect(Collectors.toList());
    }

    private String extractS3Key(String fullUrl) {
        if (fullUrl == null || fullUrl.isBlank()) return null;
        try {
            java.net.URI uri = new java.net.URI(fullUrl);
            String path = uri.getPath();
            return (path != null && path.startsWith("/")) ? path.substring(1) : path;
        } catch (Exception e) {
            return fullUrl;
        }
    }
}