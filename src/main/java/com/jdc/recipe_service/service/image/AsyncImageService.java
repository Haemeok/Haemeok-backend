package com.jdc.recipe_service.service.image;

import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeImage;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.repository.RecipeImageRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.ImageStatus;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.service.RecipeSearchService;
import com.jdc.recipe_service.util.DeferredResultHolder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.Hibernate;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.Comparator;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class AsyncImageService {

    private final RecipeRepository recipeRepository;
    private final RecipeImageRepository recipeImageRepository;
    private final RecipeIndexingService recipeIndexingService;
    private final RecipeSearchService recipeSearchService;
    private final DeferredResultHolder deferredResultHolder;
    private final GeminiImageService geminiImageService;

    private final TransactionTemplate transactionTemplate;

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
            - **Plating & Vessel:** **Select the most appropriate tableware that perfectly matches the cuisine type and the title.**
              **ALL tableware must be completely plain and unbranded:** no maker’s mark, no stamp/seal, no logo, no engraving, no embossed marks, no calligraphy, no patterns. Use solid-color blank surfaces only.
              **Served on a SINGLE plate/bowl.**
            - **Texture:** Render the food texture realistically based on the cooking method. Enhance the glistening details of oils, sauces, or moisture to make it look freshly cooked and steaming hot.
            
            **[Technical Quality]**
            - Shot on iPhone 15 Pro Max, social media aesthetic, Instagram food porn style, sharp focus on food, natural depth of field, vivid colors.
            
            **[ABSOLUTE NO-TEXT RULE]**
            - The image must contain ZERO text of any kind.
            - No Hangul/Korean, no English, no numbers, no symbols, no captions, no typography.
            - No logos, no brands, no labels, no packaging, no stamps, no engraved markings on plates/cutlery.
            - No printed patterns on tablecloth/placemats/napkins that resemble letters.
            - If any text would appear, remove it completely and leave the surface blank. 
            
            **[Negative Prompts]**
            --no people, --no hands, --no arms,
            --no text, --no watermark, --no caption,
            --no logo, --no brand, --no label, --no packaging,
            --no maker mark, --no seal, --no stamp, --no engraving, --no embossed, --no calligraphy, --no patterns,
            --no side dishes, --no banchan, --no extra bowls, --no clutter,
            --no plastic look, --no blurry, --no distorted, --no cropped plate,
            --no alcohol, --no beverage, --no soju glass,
            --no crumbs, --no messy spills, --no food splatter, --no powdery seasoning piles
            """;

    private static final String FINE_DINING_PROMPT_TEMPLATE = """
            High-end fine dining food photography. Close-up shot, Michelin star style plating.
        
            **[Dish Info]**
            Title: {{TITLE}}
            Concept Description: {{DESCRIPTION}}
        
            **[Plating Details]**
            Vessel Type: {{VESSEL}}
            Plating Instructions: {{GUIDE}}
            Visual Key Points: {{VISUAL_KEYS}}
        
            **[Camera & Lighting]**
            Viewpoint: {{VIEWPOINT}}
            Lighting: {{LIGHTING}}
        
            **[Technical Specs]**
            Shot on 100mm macro lens, f/2.8 aperture, shallow depth of field, 8k resolution, ultra-realistic, highly detailed texture.
            --no cutlery, no text, no messy background, no distorted food
            """;

    private record RecipePromptData(Long userId, String prompt) {}

    public String generateAndUploadAiImage(Long recipeId) {
        log.info("▶ [AsyncImageService] Gemini 이미지 생성 시작, recipeId={}", recipeId);

        try {
            RecipePromptData promptData = transactionTemplate.execute(status -> {
                Recipe recipe = recipeRepository.findDetailWithFineDiningById(recipeId)
                        .orElseThrow(() -> new RuntimeException("Recipe not found. ID=" + recipeId));

                String finalImagePrompt = buildPromptFromRecipe(recipe);
                return new RecipePromptData(recipe.getUser().getId(), finalImagePrompt);
            });

            if (promptData == null) throw new RuntimeException("프롬프트 데이터 생성 실패");

            log.info(">>>> [GEMINI PROMPT] Recipe ID: {}, Prompt Length: {}", recipeId, promptData.prompt.length());

            List<String> imageUrls = geminiImageService.generateImageUrls(promptData.prompt, promptData.userId, recipeId);

            if (imageUrls.isEmpty()) {
                throw new RuntimeException("Gemini 응답에 이미지 URL이 없습니다.");
            }

            String fullUrl = imageUrls.get(0);
                String s3Key = fullUrl.substring(fullUrl.indexOf(".com/") + 5);

            transactionTemplate.executeWithoutResult(status -> {
                Recipe recipe = recipeRepository.findDetailWithFineDiningById(recipeId)
                        .orElseThrow(() -> new RuntimeException("Recipe not found after generation"));

                recipe.updateImageKey(s3Key);
                recipe.updateImageStatus(RecipeImageStatus.READY);
                recipe.updateIsPrivate(false);

                RecipeImage recipeImage = RecipeImage.builder()
                        .recipe(recipe)
                        .fileKey(s3Key)
                        .slot("main")
                        .status(ImageStatus.ACTIVE)
                        .build();
                recipeImageRepository.save(recipeImage);

                Hibernate.initialize(recipe.getIngredients());
                if (recipe.getTags() != null) {
                    Hibernate.initialize(recipe.getTags());
                }

                try {
                    recipeIndexingService.updateRecipe(recipe);
                } catch (Exception e) {
                    log.warn("이미지 생성 후 인덱싱 업데이트 실패 (DB는 성공함): {}", e.getMessage());
                }
            });

            RecipeDetailDto fullDto = recipeSearchService.getRecipeDetail(recipeId, promptData.userId);
            deferredResultHolder.completeAll(recipeId, ResponseEntity.ok(fullDto));

            log.info("✅ [AsyncImageService] 이미지 생성 및 저장 완료. URL: {}", fullUrl);
            return fullUrl;

        } catch (Exception e) {
            log.error("❌ [AsyncImageService] 이미지 생성 실패, recipeId={}", recipeId, e);

            try {
                transactionTemplate.executeWithoutResult(status -> {
                    recipeRepository.findById(recipeId).ifPresent(failedRecipe -> {
                        failedRecipe.updateImageKey(null);
                        failedRecipe.updateImageStatus(RecipeImageStatus.FAILED);
                        failedRecipe.updateIsPrivate(true);
                    });
                });
            } catch (Exception ex) {
                log.error("실패 상태 업데이트 중 추가 오류 발생", ex);
            }

            var errorResponse = ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
            deferredResultHolder.completeAll(recipeId, errorResponse);

            throw new RuntimeException(e);
        }
    }

    private String buildPromptFromRecipe(Recipe recipe) {
        if (recipe.getFineDiningDetails() != null) {
            log.info("⭐ [FINE_DINING] 전용 프롬프트 엔진 가동 - Recipe ID: {}", recipe.getId());
            var details = recipe.getFineDiningDetails();

            String visualKeysStr = (details.getVisualKeys() != null)
                    ? String.join(", ", details.getVisualKeys())
                    : "";

            return FINE_DINING_PROMPT_TEMPLATE
                    .replace("{{TITLE}}", recipe.getTitle())
                    .replace("{{DESCRIPTION}}", recipe.getDescription() != null ? recipe.getDescription() : "")
                    .replace("{{VESSEL}}", details.getPlatingVessel() != null ? details.getPlatingVessel() : "Elegant plate")
                    .replace("{{GUIDE}}", details.getPlatingGuide() != null ? details.getPlatingGuide() : "")
                    .replace("{{VISUAL_KEYS}}", visualKeysStr)
                    .replace("{{VIEWPOINT}}", details.getViewpoint() != null ? details.getViewpoint() : "45-degree angle")
                    .replace("{{LIGHTING}}", details.getLighting() != null ? details.getLighting() : "Professional studio lighting");

        } else {
            String allIngredients = recipe.getIngredients().stream()
                    .map(ri -> {
                        String name = ri.getIngredient() != null ? ri.getIngredient().getName() : ri.getCustomName();
                        if (name.contains("매생이")) return "fine silky green seaweed (Maesaengi)";
                        if (name.contains("순대")) return "Korean blood sausage (Sundae)";
                        if (name.contains("떡")) return "chewy rice cakes";
                        return name;
                    })
                    .collect(Collectors.joining(", "));

            if (allIngredients.isBlank()) allIngredients = recipe.getTitle();

            String allSteps = recipe.getSteps().stream()
                    .sorted(Comparator.comparingInt(RecipeStep::getStepNumber))
                    .map(step -> String.format("- Step %d (%s): %s", step.getStepNumber(), step.getAction(), step.getInstruction()))
                    .collect(Collectors.joining("\n"));

            String randomLighting = LIGHTING_OPTIONS.get(ThreadLocalRandom.current().nextInt(LIGHTING_OPTIONS.size()));
            String randomAngle = ANGLE_OPTIONS.get(ThreadLocalRandom.current().nextInt(ANGLE_OPTIONS.size()));
            String randomBackground = BACKGROUND_OPTIONS.get(ThreadLocalRandom.current().nextInt(BACKGROUND_OPTIONS.size()));

            boolean showCutlery = ThreadLocalRandom.current().nextBoolean();
            String cutleryRule;
            if (showCutlery) {
                cutleryRule = "**Analyze the dish type.** If Asian/Korean, place wooden chopsticks and a spoon. If Western, place a fork and knife. If Finger Food(Pizza), NO cutlery. Cutlery must be plain and unbranded: no engraving, no logo, no text.";
            } else {
                cutleryRule = "**NO CUTLERY.** Do NOT place any spoon, fork, chopsticks, or knife. Keep the composition clean and minimal. Focus strictly on the food.";
            }

            return DEFAULT_PROMPT_TEMPLATE
                    .replace("{{TITLE}}", recipe.getTitle())
                    .replace("{{DISH_TYPE}}", recipe.getDishType().getDisplayName())
                    .replace("{{INGREDIENTS}}", allIngredients)
                    .replace("{{STEPS}}", allSteps)
                    .replace("{{ANGLE}}", randomAngle)
                    .replace("{{LIGHTING}}", randomLighting)
                    .replace("{{BACKGROUND}}", randomBackground)
                    .replace("{{CUTLERY_RULE}}", cutleryRule)
                    .replace("{{DESCRIPTION}}", recipe.getDescription() != null ? recipe.getDescription() : "");
        }
    }
}