package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.util.S3Util;
import lombok.RequiredArgsConstructor;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class AsyncImageService {

    private final RecipeRepository recipeRepository;
    private final S3Util s3Util;
    private final GptImageService gptImageService;
    private final RecipeIndexingService recipeIndexingService;

    /**
     * 비동기로 호출되어, 레시피 ID를 바탕으로 GPT-4o 이미지 생성 API를 호출하고,
     * S3에 업로드한 뒤 Recipe 엔티티의 imageKey, imageStatus를 업데이트합니다.
     */
    @Async
    @Transactional
    public void generateAndUploadAiImageAsync(Long recipeId) {
        return;
//        // 1) 레시피 조회
//        Recipe recipe = recipeRepository.findById(recipeId)
//                .orElseThrow(() -> new RuntimeException("Recipe not found. ID=" + recipeId));
//
//        // 2) 영어 프롬프트 구성
//        String title = recipe.getTitle();
//        Integer cookingTime = recipe.getCookingTime();
//        String cookingTimePart = (cookingTime != null)
//                ? cookingTime + " minutes of cooking time. "
//                : "";
//
//        // ——— 이 부분을 수정합니다 ———
//        List<String> ingredients = recipe.getIngredients().stream()
//                .map(ri -> {
//                    // ① 엔티티에서 미리 채워 둔 englishName 필드를 가져옴
//                    String engName = null;
//                    if (ri.getIngredient() != null) {
//                        engName = ri.getIngredient().getEnglishName();
//                    }
//                    // ② englishName이 없으면 한글명(korName)으로 페일백
//                    String korName = (ri.getIngredient() != null)
//                            ? ri.getIngredient().getName()
//                            : ri.getCustomName();
//                    if (engName == null || engName.isBlank()) {
//                        engName = korName;
//                    }
//
//                    // ③ 수량(qty)과 단위(unit) 처리
//                    String quantity = ri.getQuantity();
//                    String unit = ri.getUnit();
//                    String humanUnit = switch (unit) {
//                        case "개"   -> "pcs";
//                        case "마리" -> "fish";
//                        default     -> unit;      // 영어 변환이 없으면 원래 unit 그대로
//                    };
//
//                    return engName + " (" + quantity + " " + humanUnit + ")";
//                })
//                .collect(Collectors.toList());
//
//        String ingredientsPart = ingredients.isEmpty()
//                ? ""
//                : "Key ingredients: " + String.join(", ", ingredients) + ". ";
//        // ——————————————————————————
//
//        // dishType 기반 스타일 (원래 있던 코드 그대로 두셔도 됩니다)
//        String dishTypeDisplay = recipe.getDishType().getDisplayName();
//        String stylePart;
//        if ("가지조림".equalsIgnoreCase(title)) {
//            stylePart = "This is a Korean braised eggplant and flounder dish, simmered together in a thick, glossy soy-based sauce. ";
//        } else if (dishTypeDisplay.contains("조림") || dishTypeDisplay.contains("찜")) {
//            stylePart = "This is a Korean braised dish in a thick glossy sauce with minimal broth. ";
//        } else if (dishTypeDisplay.contains("국") || dishTypeDisplay.contains("찌개") || dishTypeDisplay.contains("탕")) {
//            stylePart = "This is a Korean soup/stew with a slightly spicy broth in a bowl. ";
//        } else {
//            stylePart = "This dish is presented in typical " + dishTypeDisplay + " style. ";
//        }
//
//        // 고명 등 나머지 문구
//        String garnishPart = "Garnished with chopped green onions and thin red chili slices for color contrast. ";
//        String cameraPart = "Photographed from a top-down flat-lay perspective, filling a white bowl completely. ";
//        String backgroundPart = "Background is a simple wooden table. ";
//        String lightPart = "Shot under bright natural light for a warm and inviting look. ";
//        String photoStylePart = "High-resolution professional food photography style, highlighting texture and sheen. ";
//
//        // 최종 프롬프트 조립
//        StringBuilder promptBuilder = new StringBuilder();
//        promptBuilder.append("/no_think\n\n");
//        promptBuilder.append("A white deep bowl showcasing \"").append(title).append("\". ");
//        promptBuilder.append(ingredientsPart);
//        promptBuilder.append(cookingTimePart);
//        promptBuilder.append(stylePart);
//        promptBuilder.append(garnishPart);
//        promptBuilder.append(cameraPart);
//        promptBuilder.append(backgroundPart);
//        promptBuilder.append(lightPart);
//        promptBuilder.append(photoStylePart);
//
//        String imagePrompt = promptBuilder.toString();
//
//        // (테스트용) 콘솔에 찍어보기
//        System.out.println("[DEBUG] imagePrompt = " + imagePrompt);
//
//        try {
//            // 3) GPT-4o 이미지 생성 요청
//            List<String> externalUrls = gptImageService.generateImageUrls(imagePrompt, 1, "1024x1024");
//            if (!externalUrls.isEmpty()) {
//                String externalUrl = externalUrls.get(0);
//                Long userId = recipe.getUser().getId();
//                String s3Key = String.format("recipes/%d/%d/main.jpg", userId, recipe.getId());
//
//                // 4) 외부 URL 다운로드 → S3 업로드
//                String storedKey = s3Util.uploadFromUrl(externalUrl, s3Key);
//
//                // 5) 엔티티 업데이트
//                recipe.updateImageKey(storedKey);
//                recipe.updateImageStatus(RecipeImageStatus.READY);
//                recipe.updateIsPrivate(false);
//
//                // 6) 색인 갱신
//                recipeIndexingService.updateRecipe(recipe);
//            } else {
//                recipe.updateImageStatus(RecipeImageStatus.FAILED);
//            }
//        } catch (Exception e) {
//            recipe.updateImageStatus(RecipeImageStatus.FAILED);
//            System.err.println("Async AI image generation failed: " + e.getMessage());
//        }
    }
}
