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

    private final RecipeRepository      recipeRepository;
    private final S3Util                s3Util;
    private final GptImageService       gptImageService;
    private final RecipeIndexingService recipeIndexingService;
    private final RecipeSearchService   recipeSearchService;
    private final DeferredResultHolder  deferredResultHolder;

    private static final Map<DishType, String> STYLE_PROMPTS = Map.ofEntries(
            entry(DishType.SOUP_STEW,       "A top-down flat-lay photo of a bubbling Korean stew in a traditional earthenware pot (ttukbaegi). The lighting is bright natural window light, creating a warm and cozy aesthetic. "),
            entry(DishType.RICE_NOODLE,     "An overhead shot of a colorful bowl of Korean rice or noodles. The image has soft natural light and a clean, minimal wooden background. "),
            entry(DishType.STEAMED_BRAISED, "A top-down view of a steaming Korean braised dish in a deep bowl, emphasizing rich, saturated colors and a homey feel. "),
            entry(DishType.FRYING,          "A 45-degree angle shot of a sizzling stir-fry on a white plate. The photo has a shallow depth of field, rich saturated colors, and a subtle vignette. "),
            entry(DishType.FRIED_PAN,       "A 45-degree angle shot of a flat round Korean pancake with golden crispy edges on a white plate, featuring a shallow depth of field and warm tones. "),
            entry(DishType.GRILL,           "A dramatic side-angle shot of grilled meat on a dark grill plate, with lighting that highlights the char marks. "),
            entry(DishType.SALAD,           "An overhead flat-lay of a fresh Korean salad on a white plate, showcasing vibrant greens under bright natural light. "),
            entry(DishType.PICKLE,          "A close-up of colorful pickled vegetables in a small ceramic bowl, with soft diffused light and rustic styling. "),
            entry(DishType.OVEN,            "A 45-degree angle shot of a baked Korean dish in a ceramic baking dish, bathed in warm golden tones from natural window light. "),
            entry(DishType.RAW,             "An overhead view of a sashimi-style raw dish on a white plate, with crisp focus against a minimalist background. "),
            entry(DishType.DESSERT,         "A single, elegant Korean dessert presented on a small, minimalist ceramic plate. The photo is a close-up from a 45-degree angle, highlighting the dessert's delicate textures. Soft, natural light with a clean, light-colored background. ")
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

        String recipeTitle = recipe.getTitle();
        if (recipeTitle == null || recipeTitle.trim().isEmpty()) {
            log.error("❌ 레시피명이 없어 이미지 생성 불가, recipeId={}", recipeId);
            recipe.updateImageStatus(RecipeImageStatus.FAILED);
            recipeRepository.save(recipe);
            return;
        }

        DishType type = recipe.getDishType();

        String mainIngredientsDesc = recipe.getIngredients().stream()
                .limit(3)
                .map(ri -> {
                    String name = ri.getIngredient() != null ? ri.getIngredient().getEnglishName() : ri.getCustomName();
                    if (name == null || name.trim().isEmpty()) {
                        return ri.getIngredient() != null ? ri.getIngredient().getName() : ri.getCustomName();
                    }
                    return name;
                })
                .filter(name -> name != null && !name.trim().isEmpty())
                .collect(Collectors.joining(", "));

        String dishDescription = String.format(
                "A photorealistic, magazine-quality image of a finished and plated Korean dish called \"%s\". It is a delicious-looking %s, fully cooked and ready to eat, which prominently features %s. ",
                recipeTitle,
                type.getDisplayName(),
                mainIngredientsDesc
        );

        String stylePart = STYLE_PROMPTS.getOrDefault(
                type,
                "The dish is presented beautifully on a plate. "
        );

        String photographicStyle = "Hyper-detailed, high-resolution food photography, highlighting texture and sheen. The composition is centered, with the dish filling most of the frame. 4:3 aspect ratio. Shot with a subtle warm Instagram-style filter and slight film grain for an inviting look. ";

        String negativePrompt = "Negative prompt: no separate or raw ingredients on the side, no props, no utensils, no people, no text or watermarks.";

        String imagePrompt = dishDescription + stylePart + photographicStyle + negativePrompt;

        log.info("▶ 생성될 이미지 프롬프트: {}", imagePrompt);

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