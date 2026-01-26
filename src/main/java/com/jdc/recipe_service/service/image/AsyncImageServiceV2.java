package com.jdc.recipe_service.service.image;

import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.service.RecipeSearchService;
import com.jdc.recipe_service.util.DeferredResultHolder;
import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

import static java.util.Map.entry;

@Service
@RequiredArgsConstructor
@Slf4j
public class AsyncImageServiceV2 {

    private final RecipeRepository recipeRepository;
    private final S3Util s3Util;
    private final RecipeIndexingService recipeIndexingService;
    private final RecipeSearchService recipeSearchService;
    private final DeferredResultHolder deferredResultHolder;
    private final NanoBananaImageService nanoBananaImageService;

    private static final String NEGATIVE_PROMPT =
            "Avoid text, logos, watermarks, signatures, disembodied hands, human fingers, ugly compositions, bad lighting, and cluttered backgrounds. The main dish must be in sharp focus.";

    private static final Map<DishType, List<String>> STYLE_PROMPTS = Map.ofEntries(
            entry(DishType.SOUP_STEW, List.of(
                    "A bubbling Korean stew in a rustic earthenware pot, styled on a wooden table with a small linen napkin and chopsticks",
                    "A hearty Korean stew in a minimalist white bowl with steam rising",
                    "A vibrant Korean stew in a black stone bowl, garnished with floating green onions and sesame seeds"
            )),
            entry(DishType.RICE_NOODLE, List.of(
                    "A vibrant bowl of Korean rice noodles on a textured stone slab with a small bowl of dipping sauce",
                    "A minimalist rice noodle dish in a clean white ceramic bowl",
                    "A colorful rice noodle plate with fresh herbs and chilies"
            )),
            entry(DishType.STEAMED_BRAISED, List.of(
                    "A steaming Korean braised dish in a deep bowl, artfully placed on a slate slab with a sprig of fresh herbs",
                    "A succulent braised stew in a white ceramic bowl, garnished with sesame seeds",
                    "A rich Korean braised dish in a rustic clay pot with steam rising"
            )),
            entry(DishType.FRYING, List.of(
                    "A colorful stir-fry on a ceramic plate with glossy sauce",
                    "A vibrant vegetable stir-fry on a modern plate, rim-lit to highlight textures",
                    "A sizzling stir-fry in a cast-iron pan, with fresh veggies under soft overhead light"
            )),
            entry(DishType.FRIED_PAN, List.of(
                    "A golden Korean pancake on a white plate with a small dipping dish",
                    "A crispy scallion pancake on a ceramic plate with garnish of spring onions and chili threads",
                    "A fluffy Korean pancake on a slate platter with a side of soy dipping sauce"
            )),
            entry(DishType.GRILL, List.of(
                    "Juicy grilled meat on a black slate platter with emphasized char marks and texture",
                    "A succulent barbecue cut on a wooden board with parsley garnish",
                    "Grilled skewers arranged on a rustic plate with charred edges"
            )),
            entry(DishType.SALAD, List.of(
                    "A fresh Korean salad on a marble surface with scattered cherry tomatoes and drifted spinach leaves",
                    "A colorful mixed salad in a glass bowl with a small jar of dressing",
                    "A deconstructed salad on a white plate with microgreens and a drizzle of vinaigrette"
            )),
            entry(DishType.PICKLE, List.of(
                    "Colorful pickled vegetables in a small ceramic bowl with soft diffused daylight",
                    "Assorted Korean pickles artfully arranged on a wooden board",
                    "A selection of pickled side dishes on a slate platter with vibrant colors"
            )),
            entry(DishType.OVEN, List.of(
                    "A baked Korean dish in a ceramic baking dish placed on a linen cloth",
                    "A golden roasted casserole in a white baking tray",
                    "A cheesy baked dish in a cast-iron skillet with melted topping"
            )),
            entry(DishType.RAW, List.of(
                    "Sashimi-style raw dish on a white plate with a side of soy sauce",
                    "Thinly sliced raw fish arranged on a bamboo board with wasabi and ginger garnish",
                    "A fresh raw seafood platter on a slate surface with decorative greenery"
            )),
            entry(DishType.DESSERT, List.of(
                    "An elegant Korean dessert on a ceramic plate accentuated by soft natural window light",
                    "A delicate sweet treat on a glass plate with powdered sugar and mint leaf garnish",
                    "A plated dessert on a marble slab with fruit coulis and edible flowers"
            ))
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

    private static final List<String> BACKGROUNDS = List.of(
            "set on a marble countertop",
            "placed on a dark rustic table",
            "in a bright, sunlit kitchen setting",
            "with a clean, minimalist background"
    );

    private static final List<String> CAMERA_LENSES = List.of(
            "with an 85mm lens",
            "with a 50mm prime lens",
            "with a macro lens for intricate detail"
    );

    private static final List<String> PHOTO_AESTHETICS = List.of(
            "hyper-detailed, award-winning food photography",
            "magazine quality, professional food photography",
            "cinematic, photorealistic food shot"
    );

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    public String generateImageUrl(String key) {
        return key == null ? null :
                String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    @Async
    @Transactional
    public CompletableFuture<String> generateAndUploadAiImageAsync(Long recipeId) {
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
                    "A high-resolution image of \"%s\", a Korean dish featuring %s",
                    recipeTitle, mainIng
            );

            if (mainIng.isBlank()) {
                dishDesc = String.format("A high-resolution image of \"%s\", a Korean dish", recipeTitle);
            }

            List<String> styles = STYLE_PROMPTS.getOrDefault(
                    type,
                    List.of("Styled simply on a plate with minimal props")
            );
            String baseStyle = styles.get(ThreadLocalRandom.current().nextInt(styles.size()));

            String background = BACKGROUNDS.get(ThreadLocalRandom.current().nextInt(BACKGROUNDS.size()));
            String sceneDescription;

            boolean hasPlatingInfo = baseStyle.matches("(?i).*\\b(bowl|plate|pot|platter|board|dish|pan|skillet)\\b.*");

            if (hasPlatingInfo) {
                sceneDescription = background;
            } else {
                String platingSurface = PLATING_SURFACES.get(ThreadLocalRandom.current().nextInt(PLATING_SURFACES.size()));
                sceneDescription = String.format("Plated on %s, %s", platingSurface, background);
            }

            String randomAngle = CAMERA_ANGLES.get(ThreadLocalRandom.current().nextInt(CAMERA_ANGLES.size()));
            String randomLighting = LIGHTING_STYLES.get(ThreadLocalRandom.current().nextInt(LIGHTING_STYLES.size()));
            String randomLens = CAMERA_LENSES.get(ThreadLocalRandom.current().nextInt(CAMERA_LENSES.size()));
            String randomAesthetic = PHOTO_AESTHETICS.get(ThreadLocalRandom.current().nextInt(PHOTO_AESTHETICS.size()));

            String photographicStyle = String.format(
                    "%s, freshly made, richly textured, appetizing gloss, shot with %s at %s, under %s. Use shallow depth of field (f/1.8) and slight film grain",
                    randomAesthetic, randomLens, randomAngle, randomLighting
            );

            String visualEmphasis = "";
            if (!mainIng.isBlank()) {
                visualEmphasis = String.format("Ensure the main ingredients (%s) are visibly prominent and clearly the focus of the dish.", mainIng);
            }

            String imagePrompt = String.join(". ",
                    dishDesc,
                    baseStyle,
                    visualEmphasis,
                    sceneDescription,
                    photographicStyle,
                    NEGATIVE_PROMPT + "."
            );

            log.info("Generated Image Prompt: {}", imagePrompt);

            Long userId = recipe.getUser().getId();

            List<String> imageUrls = nanoBananaImageService.generateImageUrls(imagePrompt, userId, recipeId);
            if (imageUrls.isEmpty()) {
                throw new RuntimeException("AI image generation returned no data.");
            }

            String imageUrl = imageUrls.get(0);
            String s3Key = imageUrl.substring(imageUrl.indexOf(".com/") + 5);

            recipe.updateImageKey(s3Key);
            recipe.updateImageStatus(RecipeImageStatus.READY);
            recipe.updateIsPrivate(false);
            recipeRepository.save(recipe);

            recipeIndexingService.updateRecipe(recipe);
            RecipeDetailDto fullDto = recipeSearchService.getRecipeDetail(recipeId, null);
            deferredResultHolder.completeAll(recipeId, ResponseEntity.ok(fullDto));

            return CompletableFuture.completedFuture(imageUrl);

        } catch (Exception e) {
            log.error("❌ [AsyncImageService] 예외 발생, recipeId={}", recipeId, e);
            try {
                recipeRepository.findById(recipeId).ifPresent(recipe -> {
                    recipe.updateImageStatus(RecipeImageStatus.FAILED);
                    recipeRepository.save(recipe);
                });
            } catch (Exception dbEx) {
                log.error("DB 업데이트 중 오류", dbEx);
            }
            try {
                RecipeDetailDto failDto = recipeSearchService.getRecipeDetail(recipeId, null);
                deferredResultHolder.completeAll(recipeId, ResponseEntity.ok(failDto));
            } catch (Exception dtoEx) {
                log.error("실패 응답 생성 중 오류", dtoEx);
            }

            return CompletableFuture.failedFuture(e);
        }
    }
}