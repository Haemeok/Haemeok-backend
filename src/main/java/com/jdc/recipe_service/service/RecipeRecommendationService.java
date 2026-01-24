package com.jdc.recipe_service.service;

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
import org.springframework.cache.annotation.Cacheable;
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
        if (key == null) return null;
        if (key.startsWith("http://") || key.startsWith("https://")) return key;
        return String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
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
     * 상세 페이지 하단 추천 리스트 반환
     *
     * 주의: 캐시 미스 시, 후보/현재 레시피의 연관관계 로딩 전략에 따라 N+1이 발생할 수 있습니다.
     */
    /**
     * 상세 페이지 하단 추천 리스트 반환
     * 전략: 카테고리 궁합이 맞는 ID 조회(Smart) -> 랜덤 셔플 -> 50개 선정 -> 상세 조회 -> 정밀 채점
     */
    @Cacheable(value = "recipeRecommendations", key = "#currentRecipeId + '-' + #limitSize", unless = "#result.isEmpty()")
    public List<RecipeSimpleStaticDto> getRecommendations(Long currentRecipeId, int limitSize) {
        if (currentRecipeId == null || limitSize <= 0) {
            return Collections.emptyList();
        }

        long startNano = 0L;
        if (log.isDebugEnabled()) {
            startNano = System.nanoTime();
        }

        Recipe currentRecipe = recipeRepository.findForRecommendationById(currentRecipeId)
                .orElseThrow(() -> new IllegalArgumentException("Recipe not found"));

        final List<DishType> bestMatches = PAIRING_MAP.get(currentRecipe.getDishType());
        final Set<TagType> currentTags = currentRecipe.getTags() == null ? Collections.emptySet() :
                currentRecipe.getTags().stream()
                        .map(RecipeTag::getTag)
                        .collect(Collectors.toSet());
        final Set<Object> currentIngredientKeys = extractIngredientKeys(currentRecipe.getIngredients());

        List<DishType> targetDishTypes = new ArrayList<>();
        if (bestMatches != null && !bestMatches.isEmpty()) {
            targetDishTypes.addAll(bestMatches);
        } else {
            targetDishTypes.add(currentRecipe.getDishType());
        }

        List<Long> candidateIds = recipeRepository.findIdsByDishTypeIn(targetDishTypes);

        if (candidateIds.size() < 50) {
            List<Long> allIds = recipeRepository.findAllPublicRecipeIds();
            Set<Long> uniqueIds = new HashSet<>(candidateIds);
            uniqueIds.addAll(allIds);
            candidateIds = new ArrayList<>(uniqueIds);
        }

        if (candidateIds.isEmpty()) return Collections.emptyList();

        Collections.shuffle(candidateIds);

        int candidatePoolSize = 50;
        List<Long> selectedIds = candidateIds.stream()
                .filter(id -> !id.equals(currentRecipeId))
                .limit(candidatePoolSize)
                .collect(Collectors.toList());

        if (selectedIds.isEmpty()) return Collections.emptyList();

        List<Recipe> candidates = recipeRepository.findCandidatesForRecommendationByIds(selectedIds);

        List<Recipe> topCandidates = candidates.stream()
                .map(candidate -> new AbstractMap.SimpleEntry<>(
                        candidate, calculateScore(bestMatches, currentTags, currentIngredientKeys, candidate)))
                .sorted((e1, e2) -> Integer.compare(e2.getValue(), e1.getValue()))
                .map(AbstractMap.SimpleEntry::getKey)
                .limit(limitSize)
                .collect(Collectors.toList());

        List<RecipeSimpleStaticDto> result = topCandidates.stream()
                .map(this::convertToDto)
                .collect(Collectors.toList());

        if (log.isDebugEnabled()) {
            long tookMs = (System.nanoTime() - startNano) / 1_000_000L;
            log.debug("[Recommendation] recipeId={}, poolSize={}, selected={}, took={}ms",
                    currentRecipeId, candidates.size(), result.size(), tookMs);
        }

        return result;
    }

    private int calculateScore(Recipe current, Recipe candidate) {
        int score = 0;

        List<DishType> bestMatches = PAIRING_MAP.get(current.getDishType());
        if (bestMatches != null && bestMatches.contains(candidate.getDishType())) {
            score += 50;
        }

        Set<TagType> currentTags = current.getTags().stream()
                .map(RecipeTag::getTag)
                .collect(Collectors.toSet());

        Set<TagType> candidateTags = candidate.getTags().stream()
                .map(RecipeTag::getTag)
                .collect(Collectors.toSet());

        if (currentTags.stream().anyMatch(candidateTags::contains)) {
            score += 30;
        }

        if (currentTags.contains(TagType.HANGOVER) && candidateTags.contains(TagType.DRINK)) score += 20;
        if (currentTags.contains(TagType.DRINK) && candidate.getDishType() == DishType.SOUP_STEW) score += 20;

        Set<Object> currentIngredientKeys = extractIngredientKeys(current.getIngredients());
        Set<Object> candidateIngredientKeys = extractIngredientKeys(candidate.getIngredients());

        boolean isIngredientOverlap = candidateIngredientKeys.stream().anyMatch(currentIngredientKeys::contains);
        if (isIngredientOverlap) {
            score -= 100;
        }

        BigDecimal rating = candidate.getAvgRating() != null ? candidate.getAvgRating() : BigDecimal.ZERO;
        score += (int) Math.round(rating.doubleValue() * 10.0);

        return score;
    }

    private int calculateScore(List<DishType> bestMatches,
                               Set<TagType> currentTags,
                               Set<Object> currentIngredientKeys,
                               Recipe candidate) {
        int score = 0;

        if (bestMatches != null && bestMatches.contains(candidate.getDishType())) {
            score += 50;
        }

        Set<TagType> candidateTags = candidate.getTags() == null ? Collections.emptySet() :
                candidate.getTags().stream()
                        .map(RecipeTag::getTag)
                        .collect(Collectors.toSet());

        if (!currentTags.isEmpty() && currentTags.stream().anyMatch(candidateTags::contains)) {
            score += 30;
        }

        if (currentTags.contains(TagType.HANGOVER) && candidateTags.contains(TagType.DRINK)) score += 20;
        if (currentTags.contains(TagType.DRINK) && candidate.getDishType() == DishType.SOUP_STEW) score += 20;

        Set<Object> candidateIngredientKeys = extractIngredientKeys(candidate.getIngredients());
        boolean isIngredientOverlap = !currentIngredientKeys.isEmpty() &&
                candidateIngredientKeys.stream().anyMatch(currentIngredientKeys::contains);
        if (isIngredientOverlap) {
            score -= 100;
        }

        BigDecimal rating = candidate.getAvgRating() != null ? candidate.getAvgRating() : BigDecimal.ZERO;
        score += (int) Math.round(rating.doubleValue() * 10.0);

        return score;
    }

    /**
     * 겹침 비교용 Key:
     * - 재료가 Ingredient를 참조하면 Ingredient ID(Long)
     * - 커스텀 재료면 customName(String)
     */
    private Set<Object> extractIngredientKeys(List<RecipeIngredient> ingredients) {
        if (ingredients == null || ingredients.isEmpty()) return Collections.emptySet();

        Set<Object> keys = new HashSet<>(ingredients.size() * 2);
        for (RecipeIngredient ri : ingredients) {
            if (ri == null) continue;

            if (ri.getIngredient() != null) {
                Long id = ri.getIngredient().getId();
                if (id != null) keys.add(id);
            } else {
                String custom = ri.getCustomName();
                if (custom != null && !custom.isBlank()) keys.add(custom);
            }
        }
        return keys;
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
                recipe.getYoutubeChannelName(),
                recipe.getYoutubeChannelId(),
                recipe.getYoutubeVideoTitle(),
                recipe.getYoutubeThumbnailUrl(),
                recipe.getYoutubeChannelProfileUrl(),
                recipe.getYoutubeSubscriberCount(),
                recipe.getYoutubeUrl(),
                recipe.isAiGenerated()
        );
    }
}