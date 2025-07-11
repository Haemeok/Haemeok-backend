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
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Base64;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadLocalRandom;
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
            entry(DishType.SOUP_STEW, "A bubbling Korean stew in a rustic earthenware pot, styled on a wooden table with a small linen napkin and chopsticks"),
            entry(DishType.RICE_NOODLE, "A vibrant bowl of Korean rice noodles on a textured stone slab, with a small bowl of dipping sauce and garnish leaves beside it"),
            entry(DishType.STEAMED_BRAISED, "A steaming Korean braised dish in a deep bowl, artfully placed on slate with a sprig of fresh herbs for contrast"),
            entry(DishType.FRYING, "A colorful stir-fry on a ceramic plate, with soft rim light highlighting the glossy sauce and blurred utensils in the background"),
            entry(DishType.FRIED_PAN, "A golden Korean pancake on a white plate, set on a dark wood board with a small dipping dish and delicate garnish"),
            entry(DishType.GRILL, "Juicy grilled meat on a black slate platter, with emphasized char marks and texture"),
            entry(DishType.SALAD, "A fresh Korean salad on a marble surface, with scattered cherry tomatoes, drifted spinach leaves, and a small jar of dressing"),
            entry(DishType.PICKLE, "Colorful pickled vegetables in a small ceramic bowl, with soft diffused daylight and gentle shadows for a rustic feel"),
            entry(DishType.OVEN, "A baked Korean dish in a ceramic baking dish, placed on a linen cloth with scattered herbs and vintage cutlery"),
            entry(DishType.RAW, "Sashimi-style raw dish on a white plate, styled with minimal pebbles and a side of soy sauce in a small bowl"),
            entry(DishType.DESSERT, "An elegant Korean dessert on a ceramic plate, accentuated by soft natural window light and a few scattered berries")
    );

    private static final List<String> LIGHTING_STYLES = List.of(
            "dramatic side window light",
            "bright and airy high-key lighting",
            "moody and cinematic low-key lighting",
            "soft diffused overhead light"
    );

    private static final List<String> CAMERA_ANGLES = List.of(
            "a top-down flat-lay angle",
            "a 45-degree angle",
            "a dynamic eye-level angle",
            "a close-up macro angle"
    );

    private static final List<String> PLATING_SURFACES = List.of(
            "a rustic wooden board",
            "a dark slate platter",
            "a simple white ceramic plate",
            "a textured stone slab"
    );

    private static final List<String> DECOR_ELEMENTS = List.of(
            "a smear of gochujang sauce and a sprig of fresh green onion",
            "a sprinkle of toasted sesame seeds and a few chili threads",
            "delicate microgreens and a drizzle of sesame oil",
            "a side of finely chopped garlic and a pinch of salt"
    );


    @Async
    @Transactional
    public CompletableFuture<Void> generateAndUploadAiImageAsync(Long recipeId) {
        log.info("▶ [AsyncImageService] 시작, recipeId={}", recipeId);

        try {
            Recipe recipe = recipeRepository.findById(recipeId)
                    .orElseThrow(() -> new RuntimeException("Recipe not found. ID=" + recipeId));

            String recipeTitle = recipe.getTitle();
            if (recipeTitle == null || recipeTitle.isBlank()) {
                throw new IllegalStateException("Recipe title cannot be blank for image generation.");
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
                    "A photorealistic, high-resolution image of \"%s\", featuring %s",
                    recipeTitle, mainIng
            );

            String platingSurface = PLATING_SURFACES.get(ThreadLocalRandom.current().nextInt(PLATING_SURFACES.size()));
            String decor = DECOR_ELEMENTS.get(ThreadLocalRandom.current().nextInt(DECOR_ELEMENTS.size()));
            String baseStyle = STYLE_PROMPTS.getOrDefault(type, "Styled simply on a plate with minimal props");
            String platingPart = String.format("Plated on %s with %s", platingSurface, decor);

            String randomAngle = CAMERA_ANGLES.get(ThreadLocalRandom.current().nextInt(CAMERA_ANGLES.size()));
            String randomLighting = LIGHTING_STYLES.get(ThreadLocalRandom.current().nextInt(LIGHTING_STYLES.size()));
            String photographicStyle = String.format(
                    "Shot at %s with %s, shallow depth of field (f/1.8) and slight film grain",
                    randomAngle, randomLighting
            );

            String negativePrompt = "no text or logos, no utensils in focus, no clutter";

            String imagePrompt = String.join(". ",
                    dishDesc,
                    baseStyle,
                    platingPart,
                    photographicStyle,
                    negativePrompt
            ) + ".";

            List<String> dataUris = gptImageService.generateImageUrls(imagePrompt, 1, "1024x1024");
            if (dataUris.isEmpty()) {
                throw new RuntimeException("AI image generation returned no data.");
            }

            String dataUri = dataUris.get(0);
            String b64 = dataUri.substring(dataUri.indexOf(',') + 1);
            byte[] imageBytes = Base64.getDecoder().decode(b64);

            Long userId = recipe.getUser().getId();
            String s3Key = String.format("images/recipes/%d/%d/main.jpg", userId, recipeId);
            s3Util.upload(imageBytes, s3Key);

            recipe.updateImageKey(s3Key);
            recipe.updateImageStatus(RecipeImageStatus.READY);
            recipe.updateIsPrivate(false);
            recipeRepository.save(recipe);

            recipeIndexingService.updateRecipe(recipe);
            RecipeDetailDto fullDto = recipeSearchService.getRecipeDetail(recipeId, null);
            deferredResultHolder.completeAll(recipeId, ResponseEntity.ok(fullDto));

            return CompletableFuture.completedFuture(null);

        } catch (Exception e) {
            log.error("❌ [AsyncImageService] 예외 발생, recipeId={}", recipeId, e);
            recipeRepository.findById(recipeId).ifPresent(recipe -> {
                recipe.updateImageStatus(RecipeImageStatus.FAILED);
                recipeRepository.save(recipe);
            });
            var errorResponse = ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(e.getMessage());
            deferredResultHolder.completeAll(recipeId, errorResponse);
            return CompletableFuture.failedFuture(e);
        }
    }
}