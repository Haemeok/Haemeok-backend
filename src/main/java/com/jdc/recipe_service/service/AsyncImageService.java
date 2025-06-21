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

import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.util.Map.entry;

@Service
@RequiredArgsConstructor
@Slf4j
public class AsyncImageService {

    private final RecipeRepository      recipeRepository;
    private final S3Util                s3Util;
    private final GptImageService       gptImageService;
    private final RecipeIndexingService recipeIndexingService;
    private final RecipeSearchService   recipeSearchService;
    private final DeferredResultHolder  deferredResultHolder;

    private static final Map<DishType, String> STYLE_PROMPTS = Map.ofEntries(
            entry(DishType.SOUP_STEW,
                    "A top-down flat-lay of a bubbling Korean stew in a rustic earthenware pot, styled on a wooden table with a small linen napkin and chopsticks."),
            entry(DishType.RICE_NOODLE,
                    "An overhead shot of a vibrant bowl of Korean rice noodles on a textured stone slab, with a small bowl of dipping sauce and garnish leaves beside it."),
            entry(DishType.STEAMED_BRAISED,
                    "A top-down view of a steaming Korean braised dish in a deep bowl, artfully placed on slate with a sprig of fresh herbs for contrast."),
            entry(DishType.FRYING,
                    "A 45° angle shot of a colorful stir-fry on a ceramic plate, with soft rim light highlighting the glossy sauce and blurred utensils in the background."),
            entry(DishType.FRIED_PAN,
                    "A 45° angle shot of a golden Korean pancake on a white plate, set on a dark wood board with a small dipping dish and delicate garnish."),
            entry(DishType.GRILL,
                    "A side-angle shot of juicy grilled meat on a black slate platter, lit by warm directional light to emphasize char marks and texture."),
            entry(DishType.SALAD,
                    "An overhead flat-lay of a fresh Korean salad on a marble surface, with scattered cherry tomatoes, drifted spinach leaves, and a small jar of dressing."),
            entry(DishType.PICKLE,
                    "A close-up of colorful pickled vegetables in a small ceramic bowl, with soft diffused daylight and gentle shadows for a rustic feel."),
            entry(DishType.OVEN,
                    "A 45° angle shot of a baked Korean dish in a ceramic baking dish, placed on a linen cloth with scattered herbs and vintage cutlery."),
            entry(DishType.RAW,
                    "An overhead sashimi-style raw dish on a white plate, styled with minimal pebbles and a side of soy sauce in a small bowl."),
            entry(DishType.DESSERT,
                    "A close-up 45° shot of an elegant Korean dessert on a ceramic plate, accentuated by soft natural window light and a few scattered berries.")
    );

    @Async
    @Transactional
    public void generateAndUploadAiImageAsync(Long recipeId) {
        log.info("▶ [AsyncImageService] 시작, recipeId={}", recipeId);

        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new RuntimeException("Recipe not found. ID=" + recipeId));

        String recipeTitle = recipe.getTitle();
        if (recipeTitle == null || recipeTitle.isBlank()) {
            recipe.updateImageStatus(RecipeImageStatus.FAILED);
            recipeRepository.save(recipe);
            return;
        }

        DishType type = recipe.getDishType();
        String mainIng = recipe.getIngredients().stream()
                .limit(3)
                .map(ri -> {
                    var ing = ri.getIngredient();
                    String name = ing != null ? ing.getEnglishName() : ri.getCustomName();
                    return (name == null || name.isBlank())
                            ? (ing != null ? ing.getName() : ri.getCustomName())
                            : name;
                })
                .filter(n -> n != null && !n.isBlank())
                .collect(Collectors.joining(", "));

        String dishDesc = String.format(
                "A photorealistic, magazine-quality image of a plated Korean dish called \"%s\", which prominently features %s. ",
                recipeTitle, type.getDisplayName(), mainIng
        );

        String stylePart = STYLE_PROMPTS.getOrDefault(
                type,
                "The dish is styled beautifully on a simple plate with minimal props."
        );

        String photographicStyle =
                "High-resolution food photography with a shallow depth of field, " +
                        "dramatic side window light casting soft shadows, " +
                        "a moody vignette around the edges, " +
                        "and slight film grain for a warm, editorial look. ";

        String negativePrompt =
                "Negative prompt: no utensils in focus, no text or logos, no separate ingredients lying around.";

        String imagePrompt = dishDesc + stylePart + photographicStyle + negativePrompt;

        try {
            List<String> dataUris = gptImageService.generateImageUrls(imagePrompt, 1, "1024x1024");
            if (dataUris.isEmpty()) {
                recipe.updateImageStatus(RecipeImageStatus.FAILED);
                recipeRepository.save(recipe);
                return;
            }

            String dataUri    = dataUris.get(0);
            String b64        = dataUri.substring(dataUri.indexOf(',') + 1);
            byte[] imageBytes = Base64.getDecoder().decode(b64);

            Long   userId = recipe.getUser().getId();
            String s3Key  = String.format("images/recipes/%d/%d/main.jpg", userId, recipeId);
            s3Util.upload(imageBytes, s3Key);

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
            throw new RuntimeException("Async AI image generation failed", e);
        }
    }
}

