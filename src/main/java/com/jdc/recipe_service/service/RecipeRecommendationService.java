package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RecipeTag;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RecipeTagRepository;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class RecipeRecommendationService {

    private final RecipeRepository recipeRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeTagRepository recipeTagRepository;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    private String generateImageUrl(String key) {
        return key == null ? null :
                String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }


    private static final Map<DishType, List<DishType>> PAIRING_MAP = new EnumMap<>(DishType.class);

    static {
        // 1. 구이(고기/메인) ↔ 면, 찌개, 샐러드, 피클
        PAIRING_MAP.put(DishType.GRILL, List.of(DishType.RICE_NOODLE, DishType.SOUP_STEW, DishType.SALAD, DishType.PICKLE));

        // 2. 볶음/튀김(기름짐) ↔ 국물, 샐러드, 절임
        PAIRING_MAP.put(DishType.FRYING, List.of(DishType.SOUP_STEW, DishType.SALAD, DishType.PICKLE, DishType.RICE_NOODLE));
        PAIRING_MAP.put(DishType.FRIED_PAN, List.of(DishType.SOUP_STEW, DishType.SALAD, DishType.PICKLE, DishType.RICE_NOODLE));

        // 3. 국/찌개(국물) ↔ 구이, 볶음, 조림
        PAIRING_MAP.put(DishType.SOUP_STEW, List.of(DishType.GRILL, DishType.FRYING, DishType.STEAMED_BRAISED, DishType.FRIED_PAN));

        // 4. 밥/면(탄수화물) ↔ 샐러드, 구이, 튀김
        PAIRING_MAP.put(DishType.RICE_NOODLE, List.of(DishType.SALAD, DishType.GRILL, DishType.FRIED_PAN, DishType.PICKLE));

        // 5. 찜/조림 ↔ 국물, 밥/면
        PAIRING_MAP.put(DishType.STEAMED_BRAISED, List.of(DishType.SOUP_STEW, DishType.RICE_NOODLE));

        // 6. 오븐요리 ↔ 파스타, 샐러드
        PAIRING_MAP.put(DishType.OVEN, List.of(DishType.RICE_NOODLE, DishType.SALAD, DishType.SOUP_STEW));

        // 7. 샐러드/절임 ↔ 모든 메인 요리
        PAIRING_MAP.put(DishType.SALAD, List.of(DishType.GRILL, DishType.RICE_NOODLE, DishType.FRYING, DishType.OVEN));
        PAIRING_MAP.put(DishType.PICKLE, List.of(DishType.GRILL, DishType.FRYING, DishType.FRIED_PAN, DishType.RICE_NOODLE));

        // 8. 디저트 ↔ 가벼운 식사류 (브런치 느낌)
        PAIRING_MAP.put(DishType.DESSERT, List.of(DishType.RICE_NOODLE, DishType.SALAD));

        // 9. 생식/회 ↔ 탕, 튀김
        PAIRING_MAP.put(DishType.RAW, List.of(DishType.SOUP_STEW, DishType.FRIED_PAN));
    }

    /**
     * 상세 페이지 하단 추천 리스트 반환 (유저 좋아요 정보 포함)
     */
    public List<RecipeSimpleStaticDto> getRecommendations(Long currentRecipeId, int limitSize) {

        Recipe currentRecipe = recipeRepository.findWithAllRelationsById(currentRecipeId)
                .orElseThrow(() -> new IllegalArgumentException("Recipe not found"));

        List<Recipe> candidates = recipeRepository.findCandidatesForRecommendation(PageRequest.of(0, 100));

        if (candidates.isEmpty()) return Collections.emptyList();

        List<Long> candidateIds = candidates.stream().map(Recipe::getId).toList();

        Map<Long, List<RecipeTag>> tagsMap = recipeTagRepository.findByRecipeIdIn(candidateIds)
                .stream().collect(Collectors.groupingBy(rt -> rt.getRecipe().getId()));

        Map<Long, List<RecipeIngredient>> ingredientsMap = recipeIngredientRepository.findByRecipeIdIn(candidateIds)
                .stream().collect(Collectors.groupingBy(ri -> ri.getRecipe().getId()));

        candidates.forEach(recipe -> {
            recipe.setTags(new HashSet<>(tagsMap.getOrDefault(recipe.getId(), Collections.emptyList())));
            recipe.setIngredients(ingredientsMap.getOrDefault(recipe.getId(), Collections.emptyList()));
        });

        int shufflePoolSize = limitSize * 2;

        List<Recipe> topCandidates = candidates.stream()
                .filter(candidate -> !candidate.getId().equals(currentRecipeId))
                .map(candidate -> new AbstractMap.SimpleEntry<>(candidate, calculateScore(currentRecipe, candidate)))
                .sorted((e1, e2) -> Integer.compare(e2.getValue(), e1.getValue()))
                .limit(shufflePoolSize)
                .map(AbstractMap.SimpleEntry::getKey)
                .collect(Collectors.toList());

        Collections.shuffle(topCandidates);

        List<RecipeSimpleStaticDto> result = topCandidates.stream()
                .limit(limitSize)
                .map(this::convertToDto)
                .collect(Collectors.toList());

        return result;
    }

    /**
     *  점수 채점기 (Scoring Logic)
     */
    private int calculateScore(Recipe current, Recipe candidate) {
        int score = 0;

        // 1. [카테고리 궁합] (+50점)
        List<DishType> bestMatches = PAIRING_MAP.get(current.getDishType());
        if (bestMatches != null && bestMatches.contains(candidate.getDishType())) {
            score += 50;
        }

        // 2. [태그 분위기 일치] (+30점)
        Set<TagType> currentTags = current.getTags().stream().map(RecipeTag::getTag).collect(Collectors.toSet());
        Set<TagType> candidateTags = candidate.getTags().stream().map(RecipeTag::getTag).collect(Collectors.toSet());

        if (currentTags.stream().anyMatch(candidateTags::contains)) {
            score += 30;
        }

        // [보너스] 술안주 ↔ 해장/국물 조합
        if (currentTags.contains(TagType.HANGOVER) && candidateTags.contains(TagType.DRINK)) score += 20;
        if (currentTags.contains(TagType.DRINK) && candidate.getDishType() == DishType.SOUP_STEW) score += 20;

        // 3. [재료 중복 회피] (-100점)
        Set<String> currentIngredients = extractIngredientNames(current.getIngredients());
        Set<String> candidateIngredients = extractIngredientNames(candidate.getIngredients());

        boolean isIngredientOverlap = candidateIngredients.stream()
                .anyMatch(currentIngredients::contains);

        if (isIngredientOverlap) {
            score -= 100;
        }

        // 4. [평점 가산] (평점 * 10점)
        BigDecimal rating = candidate.getAvgRating() != null ? candidate.getAvgRating() : BigDecimal.ZERO;
        score += rating.doubleValue() * 10;

        return score;
    }

    private Set<String> extractIngredientNames(List<RecipeIngredient> ingredients) {
        return ingredients.stream()
                .map(ri -> {
                    if (ri.getIngredient() != null) {
                        return ri.getIngredient().getName();
                    }
                    return ri.getCustomName();
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
    }

    private RecipeSimpleStaticDto convertToDto(Recipe recipe) {
        return new RecipeSimpleStaticDto(
                recipe.getId(),
                recipe.getTitle(),
                generateImageUrl(recipe.getImageKey()),
                recipe.getUser().getId(),
                recipe.getUser().getNickname(),
                recipe.getUser().getProfileImage(),
                recipe.getCreatedAt(),
                recipe.getCookingTime(),
                (long) (recipe.getLikeCount() != null ? recipe.getLikeCount() : 0),
                recipe.getAvgRating() != null ? recipe.getAvgRating() : BigDecimal.ZERO,
                recipe.getRatingCount(),
                recipe.getYoutubeUrl()
        );
    }
}