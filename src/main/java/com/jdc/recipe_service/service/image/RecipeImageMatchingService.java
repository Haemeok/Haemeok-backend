package com.jdc.recipe_service.service.image;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

@Slf4j
@Service
@RequiredArgsConstructor
public class RecipeImageMatchingService {

    private final RecipeRepository recipeRepository;

    /**
     * AI 키워드 + DishType으로 기존 레시피의 썸네일(ImageKey)을 찾아옵니다.
     */
    public String findMatchingImageKey(List<String> keywords, DishType dishType) {
        if (keywords == null || keywords.isEmpty() || dishType == null) return null;

        for (String keyword : keywords) {
            if (keyword == null || keyword.isBlank()) continue;

            String cleanKeyword = keyword.replaceAll("\\s+", "");
            log.info("🔍 [이미지 매칭] 키워드: '{}', 카테고리: '{}' 검색 시도", cleanKeyword, dishType.getDisplayName());

            long totalCandidates = recipeRepository.countCandidateRecipes(cleanKeyword, dishType);

            if (totalCandidates > 0) {
                int randomOffset = ThreadLocalRandom.current().nextInt((int) totalCandidates);

                Page<Recipe> recipePage = recipeRepository.findCandidateRecipesByKeywordAndDishType(
                        cleanKeyword, dishType, PageRequest.of(randomOffset, 1));

                if (recipePage.hasContent()) {
                    String foundImageKey = recipePage.getContent().get(0).getImageKey();
                    log.info("🎯 [이미지 랜덤 매칭 성공!] (총 {}개의 후보 중 {}번째 당첨) -> {}",
                            totalCandidates, randomOffset, foundImageKey);
                    return foundImageKey;
                }
            }
        }
        log.info("⚠️ [이미지 매칭 실패] 매칭되는 썸네일을 찾을 수 없습니다.");
        return null;
    }

    /**
     * 매칭 실패 시 요리 카테고리에 맞는 기본 S3 이미지 Key를 반환합니다.
     */
    public String getDefaultImageKeyForDishType(String dishTypeDisplayName) {
        try {
            DishType dishType = DishType.fromDisplayName(dishTypeDisplayName);
            String typeCode = dishType.name().toLowerCase();

            return "images/default-image/" + typeCode + ".webp";
        } catch (Exception e) {
            log.warn("알 수 없는 DishType: {}. 공통 기본 이미지를 반환합니다.", dishTypeDisplayName);
            return "images/default-image/default.webp";
        }
    }
}