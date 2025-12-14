package com.jdc.recipe_service.facade;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.DailyQuotaService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.SurveyService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.util.ActionImageService;
import com.jdc.recipe_service.util.PromptBuilderV3;
import com.jdc.recipe_service.util.UnitService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class AiRecipeFacade {

    private final GrokClientService grokClientService;
    private final RecipeService recipeService;
    private final DailyQuotaService dailyQuotaService;
    private final SurveyService surveyService;
    private final PromptBuilderV3 promptBuilder;
    private final ActionImageService actionImageService;
    private final UnitService unitService;

    private static final int MAX_TRIES = 2;
    private static final long RETRY_DELAY_MS = 500;

    /**
     * 트랜잭션 없이 AI 호출 수행 후, 저장 시점에만 트랜잭션 참여
     */
    public PresignedUrlResponse generateAndSave(RecipeWithImageUploadRequest request, RobotType robotType, Long userId) {

        if (request.getAiRequest() == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "AI 요청 정보가 없습니다.");
        }

        dailyQuotaService.consumeForUserOrThrow(userId);

        try {
            AiRecipeRequestDto aiReq = request.getAiRequest();
            aiReq.setUserId(userId);

            UserSurveyDto survey = surveyService.getSurvey(userId);
            applySurveyInfoToAiRequest(aiReq, survey);

            String prompt = promptBuilder.buildPrompt(aiReq, robotType);

            RecipeCreateRequestDto generatedDto = callAiWithRetry(prompt);

            generatedDto.setIngredients(correctIngredientUnits(generatedDto.getIngredients()));

            for (RecipeStepRequestDto step : generatedDto.getSteps()) {
                if (step.getAction() != null) {
                    String key = actionImageService.generateImageKey(robotType, step.getAction());
                    step.updateImageKey(key);
                }
            }

            RecipeWithImageUploadRequest processingRequest = RecipeWithImageUploadRequest.builder()
                    .aiRequest(aiReq)
                    .recipe(generatedDto)
                    .files(request.getFiles())
                    .build();

            return recipeService.createRecipeAndGenerateUrls(processingRequest, userId, RecipeSourceType.AI);

        } catch (Exception e) {
            dailyQuotaService.refundIfPolicyAllows(userId);
            throw e;
        }
    }

    private RecipeCreateRequestDto callAiWithRetry(String prompt) {
        RecipeCreateRequestDto generatedDto = null;
        for (int attempt = 1; attempt <= MAX_TRIES; attempt++) {
            try {
                generatedDto = grokClientService.generateRecipeJson(prompt).join();
                break;
            } catch (RuntimeException e) {
                if (attempt == MAX_TRIES) {
                    throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 생성 실패: " + e.getMessage(), e);
                }
                try {
                    Thread.sleep(RETRY_DELAY_MS);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "인터럽트 발생", ie);
                }
            }
        }

        if (generatedDto == null) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 응답 Null");
        if (CollectionUtils.isEmpty(generatedDto.getSteps())) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Step 생성 실패");
        if (CollectionUtils.isEmpty(generatedDto.getTags())) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Tag 생성 실패");

        return generatedDto;
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