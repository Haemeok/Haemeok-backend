package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.v2.comment.CommentStatusDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeDetailStatusDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStatusDto;
import com.jdc.recipe_service.domain.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeStatusService {

    private final RecipeRepository recipeRepository;
    private final RecipeLikeRepository recipeLikeRepository;
    private final RecipeFavoriteRepository recipeFavoriteRepository;
    private final RecipeRatingRepository recipeRatingRepository;
    private final RecipeCommentRepository recipeCommentRepository;
    private final RefrigeratorItemRepository refrigeratorItemRepository;
    private final CommentService commentService;

    @Transactional(readOnly = true)
    public Map<Long, RecipeDetailStatusDto> getStatuses(List<Long> recipeIds, Long userId) {
        if (recipeIds == null || recipeIds.isEmpty()) {
            return Collections.emptyMap();
        }

        Map<Long, Long> recipeLikeCounts = recipeRepository.findLikeCountsMapByIds(recipeIds);
        Map<Long, Long> recipeFavoriteCounts = recipeRepository.findFavoriteCountsMapByIds(recipeIds);

        Map<Long, List<CommentStatusDto>> commentStatusesMap = new HashMap<>();
        // remixCount는 user-agnostic이지만 H6(count==list.size) 보장 위해 status에서 fresh 계산. 단건 상세에서만.
        Map<Long, Long> remixCountMap = new HashMap<>();
        if (recipeIds.size() == 1) {
            Long recipeId = recipeIds.get(0);
            List<Long> commentIds = recipeCommentRepository.findTopNIdsByRecipeId(recipeId, Pageable.ofSize(3));

            if (!commentIds.isEmpty()) {
                List<CommentStatusDto> statuses = commentService.findCommentStatusesByCommentIds(userId, commentIds);
                commentStatusesMap.put(recipeId, statuses);
            }

            remixCountMap.put(recipeId, recipeRepository.countRemixesByOriginRecipeId(recipeId));
        }

        if (userId == null) {
            Map<Long, RecipeDetailStatusDto> statusMap = new HashMap<>();
            for (Long recipeId : recipeIds) {
                RecipeDetailStatusDto status = RecipeDetailStatusDto.builder()
                        .likeCount(recipeLikeCounts.getOrDefault(recipeId, 0L))
                        .likedByCurrentUser(false)
                        .favoriteCount(recipeFavoriteCounts.getOrDefault(recipeId, 0L))
                        .favoriteByCurrentUser(false)
                        .myRating(null)
                        .comments(commentStatusesMap.getOrDefault(recipeId, Collections.emptyList()))
                        .ingredientIdsInFridge(Collections.emptyList())
                        .clonedByMe(false)
                        .remixCount(remixCountMap.getOrDefault(recipeId, 0L))
                        .build();
                statusMap.put(recipeId, status);
            }
            return statusMap;
        }

        Set<Long> likedRecipeIds = recipeLikeRepository.findRecipeIdsByUserIdAndRecipeIdIn(userId, recipeIds);
        Set<Long> favoritedRecipeIds = recipeFavoriteRepository.findRecipeIdsByUserIdAndRecipeIdIn(userId, recipeIds);
        Map<Long, Double> myRatings = recipeRatingRepository.findRatingsMapByUserIdAndRecipeIdIn(userId, recipeIds);

        // 재료-냉장고 교집합과 clonedByMe는 상세 단건 조회에서만 채운다. 목록 배치에서는 쿼리 비용이 커서 제외.
        Map<Long, List<Long>> fridgeIngredientIdsMap = new HashMap<>();
        Map<Long, Boolean> clonedByMeMap = new HashMap<>();
        if (recipeIds.size() == 1) {
            Long recipeId = recipeIds.get(0);
            List<Long> inFridge = refrigeratorItemRepository.findIngredientIdsInFridgeForRecipe(userId, recipeId);
            fridgeIngredientIdsMap.put(recipeId, inFridge);
            clonedByMeMap.put(recipeId, recipeRepository.existsByOriginRecipeIdAndUserId(recipeId, userId));
        }

        Map<Long, RecipeDetailStatusDto> statusMap = new HashMap<>();
        for (Long recipeId : recipeIds) {

            Double myRating = myRatings.getOrDefault(recipeId, null);

            RecipeDetailStatusDto status = RecipeDetailStatusDto.builder()
                    .likeCount(recipeLikeCounts.getOrDefault(recipeId, 0L))
                    .likedByCurrentUser(likedRecipeIds.contains(recipeId))
                    .favoriteCount(recipeFavoriteCounts.getOrDefault(recipeId, 0L))
                    .favoriteByCurrentUser(favoritedRecipeIds.contains(recipeId))
                    .myRating(myRating)
                    .comments(commentStatusesMap.getOrDefault(recipeId, Collections.emptyList()))
                    .ingredientIdsInFridge(fridgeIngredientIdsMap.getOrDefault(recipeId, Collections.emptyList()))
                    .clonedByMe(clonedByMeMap.getOrDefault(recipeId, false))
                    .remixCount(remixCountMap.getOrDefault(recipeId, 0L))
                    .build();

            statusMap.put(recipeId, status);
        }
        return statusMap;
    }

    public Map<Long, RecipeSimpleStatusDto> convertToSimpleStatus(Map<Long, RecipeDetailStatusDto> detailStatusMap) {
        return detailStatusMap.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        entry -> RecipeSimpleStatusDto.builder()
                                .likedByCurrentUser(entry.getValue().isLikedByCurrentUser())
                                .favoriteByCurrentUser(entry.getValue().isFavoriteByCurrentUser())
                                .build()
                ));
    }
}