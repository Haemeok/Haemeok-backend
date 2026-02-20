package com.jdc.recipe_service.service.ai;

import com.jdc.recipe_service.domain.dto.ai.RecipeAnalysisResponseDto;
import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeNutritionDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.*;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.event.UserRecipeCreatedEvent;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.service.RecipeIngredientService;
import com.jdc.recipe_service.service.RecipeStepService;
import com.jdc.recipe_service.service.RecipeTagService;
import com.jdc.recipe_service.service.image.GeminiImageService;
import com.jdc.recipe_service.util.PricingUtil;
import com.jdc.recipe_service.util.UnitService;
import com.jdc.recipe_service.util.prompt.CostEffectivePromptBuilder;
import com.jdc.recipe_service.util.prompt.IngredientFocusPromptBuilder;
import com.jdc.recipe_service.util.prompt.NutritionPromptBuilder;
import com.jdc.recipe_service.util.prompt.RecipeAnalysisPromptBuilder;
import jakarta.persistence.EntityManager;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.math.BigDecimal;
import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeTestService {

    private final GrokClientService grokClientService;
    private final UnitService unitService;
    private final IngredientRepository ingredientRepo;
    private final RecipeRepository recipeRepository;
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
    private final RecipeImageRepository recipeImageRepository;

    private final RecipeAnalysisPromptBuilder analysisPromptBuilder;
    private final IngredientFocusPromptBuilder ingredientBuilder;
    private final CostEffectivePromptBuilder costBuilder;
    private final NutritionPromptBuilder nutritionBuilder;
    // private final FineDiningPromptBuilder fineDiningBuilder; // 필요 시 주석 해제
    private final TransactionTemplate transactionTemplate;

    private static final int DEFAULT_MARGIN_PERCENT = 30;

    private static final List<String> LIGHTING_OPTIONS = List.of(
            "Natural morning sunlight streaming through a kitchen window (Bright & Fresh)",
            "Warm cozy indoor kitchen lighting at dinner time (Homey & Inviting)",
            "Slightly direct overhead kitchen light (Realistic & Vivid)",
            "Soft afternoon daylight from the side (Natural & Airy)"
    );
    private static final List<String> ANGLE_OPTIONS = List.of(
            "High-angle POV shot (Point of View) looking down at the table as if ready to eat",
            "Casual top-down flat lay shot for Instagram",
            "Slightly tilted close-up shot focusing on the delicious texture",
            "Hand-held camera angle, slightly imperfect but authentic composition"
    );
    private static final List<String> BACKGROUND_OPTIONS = List.of(
            "Clean white marble table with soft natural texture (Modern & Chic)",
            "Warm light beige linen tablecloth (Cozy & Homey)",
            "Rustic dark wooden table with rich grain (Vintage & Mood)",
            "Bright white wooden table surface (Clean & Minimalist)"
    );
    private static final String DEFAULT_PROMPT_TEMPLATE = """
            **[Subject]**
            A realistic, high-quality food photo of "{{TITLE}}", taken with an iPhone 15 Pro Max.
            Category: {{DISH_TYPE}}.
            
            **[Cooking Analysis for Visualization]**
            **Based on the following cooking steps, infer the final look of the dish:**
            {{STEPS}}
            
            **[Visual Contents]**
            - **Key Ingredients:** {{INGREDIENTS}}.
            - **Overall Vibe:** Neat and appetizing plating, bright and fresh atmosphere. Focus strictly on the main food.
            
            **[Smart Filtering & Preparation Rules]**
            1. **Filter & Color:** Do NOT visualize water/salt/sugar/MSG separately. Use sauces (Soy, Chili, etc.) to determine color tone.
            2. **Cooked State:** All ingredients must appear fully cooked and integrated.
            3. **Ingredient Identity:** Main ingredients must retain their natural texture.
            
            **[Composition & Styling]**
            - **Angle:** {{ANGLE}}.
            - **Lighting:** {{LIGHTING}}.
            - **Background:** {{BACKGROUND}}. **Simple cutlery (spoon, chopsticks) placed neatly next to the plate is allowed.** NO clutter, NO side dishes, NO extra bowls.
            
            **[Cutlery Rule]**
            {{CUTLERY_RULE}}
            
            **[Visual Details (AI Inference)]**
            - **Plating & Vessel:** **Select the most appropriate tableware that perfectly matches the cuisine type and the title.** (e.g., Use a rustic clay pot for Korean stews, a wide elegant plate for pasta/steak, a wooden board for bakery). **Served on a SINGLE plate.**
            - **Texture:** Render the food texture realistically based on the cooking method. Enhance the glistening details of oils, sauces, or moisture to make it look freshly cooked and steaming hot.
            
            **[Technical Quality]**
            - Shot on iPhone 15 Pro Max, social media aesthetic, Instagram food porn style, sharp focus on food, natural depth of field, vivid colors.
            
            **[Negative Prompts]**
            --no people, --no human body, --no hands, --no arms, --no chopsticks held by hand, 
            --no alcohol, --no soju glass, --no beverage, 
            --no side dishes, --no banchan, --no small plates, --no bowls, --no soup, 
            --no messy piles, --no unappetizing mess, --no crumbs, 
            --no raw powder, --no distorted blurry food, --no cropped plate, --no plastic look, 
            --no text, --no watermark.
            """;

    /**
     * [TEST] 이미지/DB 저장 없이 AI 레시피 텍스트(JSON DTO)만 생성
     * AiRecipeConcept에 따라 다른 프롬프트 로직을 수행합니다.
     */
    public RecipeCreateRequestDto generateRecipeTextOnly(
            AiRecipeConcept concept,
            AiRecipeRequestDto aiReq) {

        if (aiReq == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "AI 요청 정보가 없습니다.");
        }
        if (concept == null) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "Concept 파라미터가 필요합니다.");
        }

        aiReq.setUserId(null);
        if (aiReq.getSpiceLevel() == null) aiReq.setSpiceLevel(0);
        if (aiReq.getAllergy() == null) aiReq.setAllergy("없음");
        if (aiReq.getTags() == null) aiReq.setTags(Collections.emptyList());

        log.info("AI 레시피 텍스트 전용 생성 시작. Concept: {}", concept);

        RecipeCreateRequestDto generatedDto;

        try {
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
                    String step1System = nutritionBuilder.buildStep1Prompt(aiReq);
                    String step1Trigger = "위 조건에 맞춰 JSON 결과만 출력해.";
                    String step1Json = grokClientService.generateRaw(step1System, step1Trigger).join();

                    String step2System = nutritionBuilder.buildStep2Prompt(step1Json);
                    String step2Trigger = "위 재료와 양념을 조합하여 완벽한 레시피를 JSON으로 만들어줘.";
                    generatedDto = grokClientService.generateRecipeJson(step2System, step2Trigger).join();
                }
                /* case FINE_DINING -> {
                    String systemPrompt = fineDiningBuilder.buildPrompt(aiReq);
                    String userTrigger = "위 정보를 바탕으로 최고급 레시피 JSON을 생성해줘.";
                    generatedDto = grokClientService.generateRecipeJson(systemPrompt, userTrigger).join();
                } */
                default -> throw new IllegalArgumentException("Unknown Concept: " + concept);
            }

        } catch (RuntimeException e) {
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "Grok AI 레시피 생성 실패: " + e.getMessage(), e
            );
        }

        if (generatedDto == null) {
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 응답이 null입니다.");
        }

        generatedDto.setIngredients(correctIngredientUnits(generatedDto.getIngredients()));

        calculateAndSetDtoNutritionAndCost(generatedDto);

        if (generatedDto.getSteps() == null || generatedDto.getSteps().isEmpty() ||
                generatedDto.getTags() == null || generatedDto.getTags().isEmpty()) {
            throw new CustomException(
                    ErrorCode.AI_RECIPE_GENERATION_FAILED,
                    "AI가 필수 요리 단계 또는 태그 정보를 생성하지 못했습니다."
            );
        }

        log.info("AI 레시피 텍스트 생성 성공: {}", generatedDto.getTitle());
        return generatedDto;
    }

    /**
     * [REAL - FULL LOGIC]
     * DB 저장, 이미지 생성, 이벤트 발행 등 실제 서비스 흐름을 테스트합니다.
     */
    @Transactional
    public RecipeCreateRequestDto createRealRecipeWithCustomImage(Long userId, RecipeCreateRequestDto dto) {

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

        String allIngredients = "";
        if (dto.getIngredients() != null) {
            allIngredients = dto.getIngredients().stream()
                    .map(RecipeIngredientRequestDto::getName)
                    .map(name -> {
                        if (name.contains("매생이")) return "fine silky green seaweed (Maesaengi)";
                        if (name.contains("순대")) return "Korean blood sausage (Sundae)";
                        if (name.contains("떡")) return "chewy rice cakes";
                        return name;
                    })
                    .collect(Collectors.joining(", "));
        }
        if (allIngredients.isBlank()) allIngredients = dto.getTitle();

        String allSteps = "";
        if (dto.getSteps() != null) {
            allSteps = dto.getSteps().stream()
                    .sorted(Comparator.comparingInt(RecipeStepRequestDto::getStepNumber))
                    .map(step -> String.format("- Step %d (%s): %s", step.getStepNumber(), step.getAction(), step.getInstruction()))
                    .collect(Collectors.joining("\n"));
        }

        String randomLighting = LIGHTING_OPTIONS.get(ThreadLocalRandom.current().nextInt(LIGHTING_OPTIONS.size()));
        String randomAngle = ANGLE_OPTIONS.get(ThreadLocalRandom.current().nextInt(ANGLE_OPTIONS.size()));
        String randomBackground = BACKGROUND_OPTIONS.get(ThreadLocalRandom.current().nextInt(BACKGROUND_OPTIONS.size()));
        boolean showCutlery = ThreadLocalRandom.current().nextBoolean();

        String cutleryRule;
        if (showCutlery) {
            cutleryRule = "**Analyze the dish type.** If Asian/Korean, place wooden chopsticks and a spoon. If Western, place a fork and knife. If Finger Food(Pizza), NO cutlery.";
        } else {
            cutleryRule = "**NO CUTLERY.** Do NOT place any spoon, fork, chopsticks, or knife. Keep the composition clean and minimal. Focus strictly on the food.";
        }

        String finalImagePrompt = DEFAULT_PROMPT_TEMPLATE
                .replace("{{TITLE}}", dto.getTitle())
                .replace("{{DISH_TYPE}}", dto.getDishType())
                .replace("{{INGREDIENTS}}", allIngredients)
                .replace("{{STEPS}}", allSteps)
                .replace("{{ANGLE}}", randomAngle)
                .replace("{{LIGHTING}}", randomLighting)
                .replace("{{BACKGROUND}}", randomBackground)
                .replace("{{CUTLERY_RULE}}", cutleryRule)
                .replace("{{DESCRIPTION}}", dto.getDescription());

        log.info(">>>> [SMART PROMPT GEN] Recipe ID: {}, Prompt: \n{}", recipe.getId(), finalImagePrompt);

        try {
            List<String> imageUrls = geminiImageService.generateImageUrls(finalImagePrompt, userId, recipe.getId());

            if (!imageUrls.isEmpty()) {
                String fullUrl = imageUrls.get(0);
                String s3Key = fullUrl.substring(fullUrl.indexOf(".com/") + 5);

                Recipe savedRecipe = recipeRepository.findById(recipe.getId()).orElseThrow();
                savedRecipe.updateImageKey(s3Key);
                savedRecipe.updateImageStatus(RecipeImageStatus.READY);
                savedRecipe.updateIsPrivate(false);

                RecipeImage recipeImage = RecipeImage.builder()
                        .recipe(savedRecipe)
                        .fileKey(s3Key)
                        .slot("main")
                        .status(ImageStatus.ACTIVE)
                        .build();

                recipeImageRepository.save(recipeImage);
                dto.setImageKey(fullUrl);
                recipeRepository.save(savedRecipe);
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

    /**
     * [TEST] 배치 인서트 로직
     */
    public List<RecipeCreateRequestDto> batchInsertRecipes(List<RecipeCreateRequestDto> requests, String type) {
        List<RecipeCreateRequestDto> results = new ArrayList<>();
        List<Long> targetUserIds = getTargetUserIds(type);

        if (targetUserIds.isEmpty()) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "유효한 테스트 계정 ID 범위가 아닙니다.");
        }

        for (RecipeCreateRequestDto request : requests) {
            Long randomUserId = targetUserIds.get(ThreadLocalRandom.current().nextInt(targetUserIds.size()));

            try {
                RecipeCreateRequestDto savedDto = transactionTemplate.execute(status -> {
                    log.info(">>>> [Batch Insert] Assigning Recipe '{}' to User ID: {}", request.getTitle(), randomUserId);
                    return createRealRecipeWithCustomImage(randomUserId, request);
                });

                if (savedDto != null) {
                    results.add(savedDto);
                }

            } catch (Exception e) {
                log.error(">>>> [Batch Insert Failed] Recipe: {}, Error: {}", request.getTitle(), e.getMessage());
            }
        }
        return results;
    }

    /**
     * [TEST] 특정 레시피 ID에 대해 분석(가격/팁/욕설)만 수행
     */
    public RecipeAnalysisResponseDto analyzeRecipeTest(Long recipeId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        String prompt = analysisPromptBuilder.buildAnalysisPrompt(recipe);
        log.info(">>>> [TEST] Analysis Prompt Generated: \n{}", prompt);

        return grokClientService.analyzeRecipe(prompt).join();
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

    private void calculateAndSetTotalNutrition(Recipe recipe, List<com.jdc.recipe_service.domain.entity.RecipeIngredient> ingredients) {
        java.math.BigDecimal totalCalorie = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalCarb = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalProtein = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalFat = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalSugar = java.math.BigDecimal.ZERO;
        java.math.BigDecimal totalSodium = java.math.BigDecimal.ZERO;

        for (com.jdc.recipe_service.domain.entity.RecipeIngredient ri : ingredients) {
            java.math.BigDecimal quantity = parseQuantityToBigDecimal(ri.getQuantity());

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

    private void calculateAndSetDtoNutritionAndCost(RecipeCreateRequestDto dto) {
        if (dto.getIngredients() == null || dto.getIngredients().isEmpty()) return;

        List<String> names = dto.getIngredients().stream()
                .map(RecipeIngredientRequestDto::getName)
                .collect(Collectors.toList());

        List<Ingredient> dbIngredients = ingredientRepo.findAllByNameIn(names);
        Map<String, Ingredient> ingredientMap = dbIngredients.stream()
                .collect(Collectors.toMap(Ingredient::getName, i -> i));

        BigDecimal totalCalorie = BigDecimal.ZERO;
        BigDecimal totalCarb = BigDecimal.ZERO;
        BigDecimal totalProtein = BigDecimal.ZERO;
        BigDecimal totalFat = BigDecimal.ZERO;
        BigDecimal totalSugar = BigDecimal.ZERO;
        BigDecimal totalSodium = BigDecimal.ZERO;
        BigDecimal totalCost = BigDecimal.ZERO;

        for (RecipeIngredientRequestDto ri : dto.getIngredients()) {
            BigDecimal quantity = parseQuantityToBigDecimal(ri.getQuantity());
            Ingredient dbIng = ingredientMap.get(ri.getName());

            if (dbIng != null) {
                totalCalorie = totalCalorie.add(safeMultiply(dbIng.getCalorie(), quantity));
                totalCarb = totalCarb.add(safeMultiply(dbIng.getCarbohydrate(), quantity));
                totalProtein = totalProtein.add(safeMultiply(dbIng.getProtein(), quantity));
                totalFat = totalFat.add(safeMultiply(dbIng.getFat(), quantity));
                totalSugar = totalSugar.add(safeMultiply(dbIng.getSugar(), quantity));
                totalSodium = totalSodium.add(safeMultiply(dbIng.getSodium(), quantity));

                if (dbIng.getPrice() != null) {
                    totalCost = totalCost.add(BigDecimal.valueOf(dbIng.getPrice()).multiply(quantity));
                }
            } else {
                totalCalorie = totalCalorie.add(safeBigDecimal(ri.getCustomCalories()));
                totalCarb = totalCarb.add(safeBigDecimal(ri.getCustomCarbohydrate()));
                totalProtein = totalProtein.add(safeBigDecimal(ri.getCustomProtein()));
                totalFat = totalFat.add(safeBigDecimal(ri.getCustomFat()));
                totalSugar = totalSugar.add(safeBigDecimal(ri.getCustomSugar()));
                totalSodium = totalSodium.add(safeBigDecimal(ri.getCustomSodium()));

                if (ri.getCustomPrice() != null) {
                    totalCost = totalCost.add(ri.getCustomPrice());
                }
            }
        }

        RecipeNutritionDto nutrition = RecipeNutritionDto.builder()
                .carbohydrate(totalCarb)
                .protein(totalProtein)
                .fat(totalFat)
                .sugar(totalSugar)
                .sodium(totalSodium)
                .build();
        dto.setNutrition(nutrition);
        dto.setTotalCalories(totalCalorie.doubleValue());

        int costInt = totalCost.intValue();
        dto.setTotalIngredientCost(costInt);

        if (dto.getMarketPrice() == null || dto.getMarketPrice() <= 0) {
            dto.setMarketPrice(PricingUtil.applyMargin(costInt, DEFAULT_MARGIN_PERCENT));
        }
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

    private BigDecimal safeMultiply(BigDecimal value, BigDecimal quantity) {
        if (value == null) return BigDecimal.ZERO;
        return value.multiply(quantity);
    }

    private BigDecimal safeBigDecimal(BigDecimal value) {
        return value != null ? value : BigDecimal.ZERO;
    }

    private List<Long> getTargetUserIds(String type) {
        List<Long> ids = new ArrayList<>();
        int start = 90001;
        int end = 90100;

        boolean isOdd = "odd".equalsIgnoreCase(type);
        boolean isEven = "even".equalsIgnoreCase(type);

        if (!isOdd && !isEven) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "type 파라미터는 'odd' 또는 'even'이어야 합니다.");
        }

        for (long i = start; i <= end; i++) {
            if (isOdd && (i % 2 != 0)) {
                ids.add(i);
            } else if (isEven && (i % 2 == 0)) {
                ids.add(i);
            }
        }
        return ids;
    }
}