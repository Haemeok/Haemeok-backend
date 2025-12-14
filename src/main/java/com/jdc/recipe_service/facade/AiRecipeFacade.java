package com.jdc.recipe_service.facade;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.dto.user.UserSurveyDto;
import com.jdc.recipe_service.domain.type.AiRecipeConcept;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.DailyQuotaService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.SurveyService;
import com.jdc.recipe_service.service.ai.GrokClientService;
import com.jdc.recipe_service.util.ActionImageService;
import com.jdc.recipe_service.util.UnitService;
import com.jdc.recipe_service.util.prompt.CostEffectivePromptBuilder;
import com.jdc.recipe_service.util.prompt.IngredientFocusPromptBuilder;
import com.jdc.recipe_service.util.prompt.NutritionPromptBuilder;
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
    private final ActionImageService actionImageService;
    private final UnitService unitService;

    private final IngredientFocusPromptBuilder ingredientBuilder;
    private final CostEffectivePromptBuilder costBuilder;
    private final NutritionPromptBuilder nutritionBuilder;
    //private final FineDiningPromptBuilder fineDiningBuilder;


    /**
     * 트랜잭션 없이 AI 호출 수행 후, 저장 시점에만 트랜잭션 참여
     */
    public PresignedUrlResponse generateAndSave(RecipeWithImageUploadRequest request, AiRecipeConcept concept, Long userId) {

        if (request.getAiRequest() == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "AI 요청 정보가 없습니다.");
        }

        dailyQuotaService.consumeForUserOrThrow(userId);

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
                /*case FINE_DINING -> {
                    String systemPrompt = fineDiningBuilder.buildPrompt(aiReq);
                    String userTrigger = "위 정보를 바탕으로 최고급 레시피 JSON을 생성해줘.";
                    generatedDto = grokClientService.generateRecipeJson(systemPrompt, userTrigger).join();
                }*/
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

            return recipeService.createRecipeAndGenerateUrls(processingRequest, userId, RecipeSourceType.AI);

        } catch (Exception e) {
            dailyQuotaService.refundIfPolicyAllows(userId);
            throw e;
        }
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