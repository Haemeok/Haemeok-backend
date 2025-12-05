package com.jdc.recipe_service.service.ai;

import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.*;
import com.jdc.recipe_service.event.UserRecipeCreatedEvent;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.service.RecipeIngredientService;
import com.jdc.recipe_service.service.RecipeStepService;
import com.jdc.recipe_service.service.RecipeTagService;
import com.jdc.recipe_service.service.image.GeminiImageService;
import com.jdc.recipe_service.service.image.NanoBananaImageService;
import com.jdc.recipe_service.util.PricingUtil;
import jakarta.persistence.EntityManager;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;


@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeTestServiceV2 {

    private final GrokClientService grokClientService;
    private final RecipeRepository recipeRepository;
    private final NanoBananaImageService nanoBananaImageService;
    private final UserRepository userRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepService recipeStepService;
    private final RecipeTagService recipeTagService;
    private final RecipeIngredientService recipeIngredientService;
    private final RecipeIndexingService recipeIndexingService;
    private final RecipeAnalysisService recipeAnalysisService;
    private final ApplicationEventPublisher publisher;
    private final EntityManager em;
    private final GeminiImageService geminiImageService;

    private static final int DEFAULT_MARGIN_PERCENT = 30;

    @Transactional
    public RecipeCreateRequestDto createRealRecipeWithCustomImage(Long userId, AiImageTestRequestDto request) {

        RecipeCreateRequestDto dto = request.getRequestData();
        User user = userRepository.findById(userId).orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        Recipe recipe = Recipe.builder()
                .user(user)
                .title(dto.getTitle())
                .description(dto.getDescription())
                .dishType(DishType.fromDisplayName(dto.getDishType()))
                .cookingTime(dto.getCookingTime())
                .servings(dto.getServings())
                .isAiGenerated(false)
                .isPrivate(true)
                .imageStatus(RecipeImageStatus.PENDING)
                .build();

        recipeRepository.save(recipe);

        int totalCost = recipeIngredientService.saveAll(recipe, dto.getIngredients(), RecipeSourceType.USER);
        recipe.updateTotalIngredientCost(totalCost);
        List<RecipeIngredient> savedIngredients = recipeIngredientRepository.findByRecipeId(recipe.getId());
        calculateAndSetTotalNutrition(recipe, savedIngredients);
        int marketPrice = calculateMarketPrice(dto, totalCost);
        recipe.updateMarketPrice(marketPrice);
        recipeStepService.saveAll(recipe, dto.getSteps());
        recipeTagService.saveAll(recipe, dto.getTags());

        em.flush();
        em.clear();

        String finalImagePrompt;

        if (request.getPrompt() != null && !request.getPrompt().isBlank()) {
            finalImagePrompt = request.getPrompt()
                    .replace("{{TITLE}}", dto.getTitle())
                    .replace("{{DISH_TYPE}}", dto.getDishType());

            log.info("📢 [MANUAL MODE] 수동 입력 프롬프트를 사용합니다.");

        } else {
            String rawIngredientsStr = "";
            if (dto.getIngredients() != null) {
                rawIngredientsStr = dto.getIngredients().stream()
                        .map(RecipeIngredientRequestDto::getName)
                        .filter(name -> name != null && !name.isBlank())
                        .collect(Collectors.joining(", "));
            }

            finalImagePrompt = grokClientService.generateFinalImagePrompt(
                    dto.getTitle(),
                    rawIngredientsStr,
                    dto.getDishType(),
                    dto.getDescription()
            ).join();

            log.info("🤖 [AUTO MODE] Grok V2가 프롬프트를 작성했습니다.");
        }

        log.info(">>>> [FINAL PROMPT] Recipe ID: {}, Prompt: \n{}", recipe.getId(), finalImagePrompt);

        try {
            List<String> imageUrls = generateImageWithSelectedModel(
                    request.getModel(), finalImagePrompt, userId, recipe.getId()
            );
            if (!imageUrls.isEmpty()) {
                String fullUrl = imageUrls.get(0);
                String s3Key = fullUrl.substring(fullUrl.indexOf(".com/") + 5);

                Recipe savedRecipe = recipeRepository.findById(recipe.getId()).orElseThrow();
                savedRecipe.updateImageKey(s3Key);
                savedRecipe.updateImageStatus(RecipeImageStatus.READY);
                savedRecipe.updateIsPrivate(false);
                recipeRepository.save(savedRecipe);

                dto.setImageKey(fullUrl);
            } else {
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "이미지 생성 결과 없음");
            }
        } catch (Exception e) {
            log.error("이미지 생성 실패", e);
            Recipe savedRecipe = recipeRepository.findById(recipe.getId()).orElseThrow();
            savedRecipe.updateImageStatus(RecipeImageStatus.FAILED);
            recipeRepository.save(savedRecipe);
        }

        final Long finalRecipeId = recipe.getId();
        TransactionSynchronizationManager.registerSynchronization(
                new TransactionSynchronization() {
                    @Override
                    public void afterCommit() {
                        try {
                            recipeIndexingService.indexRecipeSafelyWithRetry(finalRecipeId);
                        } catch (Exception e) {
                            log.error("인덱싱 실패", e);
                        }
                        publisher.publishEvent(new UserRecipeCreatedEvent(finalRecipeId));
                        recipeAnalysisService.analyzeRecipeAsync(finalRecipeId);
                    }
                });

        return dto;
    }

    private static int calculateMarketPrice(RecipeCreateRequestDto dto, int totalCost) {
        Integer providedMp = dto.getMarketPrice();
        int marketPrice = (providedMp != null && providedMp > 0)
                ? providedMp
                : (totalCost > 0
                ? PricingUtil.applyMargin(totalCost, PricingUtil.randomizeMarginPercent(DEFAULT_MARGIN_PERCENT))
                : 0);

        if (marketPrice < totalCost) {
            marketPrice = PricingUtil.applyMargin(totalCost, DEFAULT_MARGIN_PERCENT);
        }
        return marketPrice;
    }

    private void calculateAndSetTotalNutrition(Recipe recipe, List<RecipeIngredient> ingredients) {
        BigDecimal totalCalorie = BigDecimal.ZERO;
        BigDecimal totalCarb = BigDecimal.ZERO;
        BigDecimal totalProtein = BigDecimal.ZERO;
        BigDecimal totalFat = BigDecimal.ZERO;
        BigDecimal totalSugar = BigDecimal.ZERO;
        BigDecimal totalSodium = BigDecimal.ZERO;

        for (RecipeIngredient ri : ingredients) {
            BigDecimal quantity = parseQuantityToBigDecimal(ri.getQuantity());

            if (ri.getIngredient() != null) {
                Ingredient ing = ri.getIngredient();
                totalCalorie = totalCalorie.add(ing.getCalorie().multiply(quantity));
                totalCarb = totalCarb.add(ing.getCarbohydrate().multiply(quantity));
                totalProtein = totalProtein.add(ing.getProtein().multiply(quantity));
                totalFat = totalFat.add(ing.getFat().multiply(quantity));
                totalSugar = totalSugar.add(ing.getSugar().multiply(quantity));
                totalSodium = totalSodium.add(ing.getSodium().multiply(quantity));
            } else {
                totalCalorie = totalCalorie.add(ri.getCustomCalorie());
                totalCarb = totalCarb.add(ri.getCustomCarbohydrate());
                totalProtein = totalProtein.add(ri.getCustomProtein());
                totalFat = totalFat.add(ri.getCustomFat());
                totalSugar = totalSugar.add(ri.getCustomSugar());
                totalSodium = totalSodium.add(ri.getCustomSodium());
            }
        }
        recipe.updateNutrition(totalProtein, totalCarb, totalFat, totalSugar, totalSodium, totalCalorie);
    }


    private BigDecimal parseQuantityToBigDecimal(String quantityStr) {
        if (quantityStr == null || quantityStr.isBlank()) return BigDecimal.ZERO;
        String cleanStr = quantityStr.replaceAll("[^0-9./]", "");
        try {
            if (cleanStr.contains("/")) {
                String[] parts = cleanStr.split("/");
                if (parts.length == 2) {
                    double num = Double.parseDouble(parts[0]);
                    double den = Double.parseDouble(parts[1]);
                    if (den == 0) return BigDecimal.ZERO;
                    return BigDecimal.valueOf(num / den);
                }
            }
            return new BigDecimal(cleanStr);
        } catch (Exception e) {
            return BigDecimal.ZERO;
        }
    }

    private List<String> generateImageWithSelectedModel(ImageGenModel model, String prompt, Long userId, Long recipeId) {
        if (model == ImageGenModel.GEMINI) {
            return geminiImageService.generateImageUrls(prompt, userId, recipeId);
        }
        return nanoBananaImageService.generateImageUrls(prompt, userId, recipeId);
    }

    private BigDecimal safeMultiply(BigDecimal value, BigDecimal quantity) {
        if (value == null) return BigDecimal.ZERO;
        return value.multiply(quantity);
    }

}