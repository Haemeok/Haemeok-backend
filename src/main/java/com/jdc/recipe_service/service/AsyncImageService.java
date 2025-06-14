package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.util.DeferredResultHolder;
import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.Map.entry;

@Service
@RequiredArgsConstructor
@Slf4j
public class AsyncImageService {

    private final RecipeRepository        recipeRepository;
    private final S3Util                  s3Util;
    private final GptImageService         gptImageService;
    private final RecipeIndexingService   recipeIndexingService;
    private final RecipeSearchService     recipeSearchService;
    private final DeferredResultHolder    deferredResultHolder;

    private static final Map<DishType, String> STYLE_PROMPTS = Map.ofEntries(
            entry(DishType.SOUP_STEW,     "Bright natural window light over a bubbling Korean stew in an earthenware pot, top-down flat-lay, warm and cozy aesthetic. "),
            entry(DishType.RICE_NOODLE,   "Overhead view of a colorful bowl of Korean rice or noodles, soft natural light, clean minimal wooden background. "),
            entry(DishType.STEAMED_BRAISED, "Top-down view of a steaming Korean braided dish in a deep bowl, rich saturated colors and homey feel. "),
            entry(DishType.FRYING,        "Side-angle shot of sizzling stir-fry on a white plate, shallow depth of field, rich saturated colors with subtle vignette. "),
            entry(DishType.FRIED_PAN,     "Side-angle shot of a flat round pancake with golden crispy edges on a white plate, shallow depth of field and warm tones. "),
            entry(DishType.GRILL,         "Side-angle shot of grilled meat on a dark grill plate, dramatic lighting highlighting char marks. "),
            entry(DishType.SALAD,         "Overhead flat-lay of a fresh Korean salad on a white plate, vibrant greens and bright natural light. "),
            entry(DishType.PICKLE,        "Close-up of colorful pickled vegetables in a small ceramic bowl, soft diffused light and rustic styling. "),
            entry(DishType.OVEN,          "Side-angle shot of a baked Korean dish on a ceramic baking dish, warm golden tones with natural window light. "),
            entry(DishType.RAW,           "Overhead view of sashimi-style raw dish on a white plate, crisp focus and minimalist background. "),
            entry(DishType.DESSERT,       "Flat-lay of assorted Korean desserts on a wooden tray, delicate pastel tones, warm and cozy aesthetic. ")
    );

    @Async
    @Transactional
    public void generateAndUploadAiImageAsync(Long recipeId) {
        log.info("▶ [AsyncImageService] 시작, recipeId={}", recipeId);

        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> {
                    log.error("❌ [AsyncImageService] Recipe 조회 실패, ID={}", recipeId);
                    return new RuntimeException("Recipe not found. ID=" + recipeId);
                });

        recipe.getIngredients().size();
        recipe.getSteps().size();

        String title     = recipe.getTitle();
        Integer cookTime = recipe.getCookingTime();
        DishType type    = recipe.getDishType();

        boolean isBowl = switch (type) {
            case SOUP_STEW, RICE_NOODLE, STEAMED_BRAISED -> true;
            default -> false;
        };
        String containerPrompt = String.format(
                "%s \"%s\". ",
                isBowl ? "A white deep bowl showcasing" : "A white plate showcasing",
                title
        );

        String ingredientsPart = recipe.getIngredients().stream()
                .map(ri -> {
                    String en = ri.getIngredient() != null
                            ? ri.getIngredient().getEnglishName()
                            : ri.getCustomName();
                    if (en == null || en.isBlank()) en = ri.getIngredient().getName();
                    String qty = ri.getQuantity();
                    String unit = switch (ri.getUnit()) {
                        case "개" -> "pcs";
                        case "마리" -> "pcs of fish";
                        default -> ri.getUnit();
                    };
                    return en + " (" + qty + " " + unit + ")";
                })
                .collect(Collectors.joining(", ", "Key ingredients: ", ". "));

        String timePart = cookTime != null
                ? cookTime + " minutes of cooking time. "
                : "";

        String stylePart = STYLE_PROMPTS.getOrDefault(
                type,
                "Presented in typical " + type.getDisplayName() + " style. "
        );

        String cameraPart      = "Photographed from a 45° angle in natural window light. ";
        String compositionPart = "Centered composition, dish filling most of the frame. ";
        String ratioPart       = "4:3 aspect ratio. ";
        String backgroundPart  = "Background is a simple wooden table. ";
        String lightPart       = "Shot under bright natural light for a warm and inviting look. ";
        String photoStylePart  = "High-resolution photorealistic food photography style, highlighting texture and sheen. ";
        String filterPart      = "Include a subtle warm Instagram-style filter with slight film grain. ";

        String negativePart    = "Negative prompt: no separate ingredient items, no raw ingredients, no props, no utensils. ";

        String imagePrompt = containerPrompt
                + ingredientsPart
                + timePart
                + stylePart
                + cameraPart
                + compositionPart
                + ratioPart
                + backgroundPart
                + lightPart
                + photoStylePart
                + filterPart
                + negativePart;

        try {
            List<String> urls = gptImageService.generateImageUrls(imagePrompt, 1, "1024x1024");
            if (urls.isEmpty()) {
                log.warn("⚠️ [AsyncImageService] 이미지 URL을 하나도 못 받아옴, recipeId={}", recipeId);
                recipe.updateImageStatus(RecipeImageStatus.FAILED);
                recipeRepository.save(recipe);
                return;
            }
            String externalUrl = urls.get(0);
            Long userId = recipe.getUser().getId();
            String s3Key = String.format("recipes/%d/%d/main.jpg", userId, recipe.getId());

            log.info("⏳ [AsyncImageService] S3 업로드 시작, key={}", s3Key);
            s3Util.uploadFromUrl(externalUrl, s3Key);
            log.info("✅ [AsyncImageService] S3 업로드 완료, key={}", s3Key);

            recipe.updateImageKey(s3Key);
            recipe.updateImageStatus(RecipeImageStatus.READY);
            recipe.updateIsPrivate(false);
            recipeRepository.save(recipe);

            recipeIndexingService.updateRecipe(recipe);
            RecipeDetailDto fullDto = recipeSearchService.getRecipeDetail(recipeId, null);
            deferredResultHolder.completeAll(recipeId, ResponseEntity.ok(fullDto));
        } catch (Exception e) {
            log.error("❌ [AsyncImageService] 예외 발생, recipeId={}", recipeId, e);
            recipe.updateImageStatus(RecipeImageStatus.FAILED);
            recipeRepository.save(recipe);
            throw new RuntimeException("Async AI image generation failed: " + e.getMessage(), e);
        }
    }
}
