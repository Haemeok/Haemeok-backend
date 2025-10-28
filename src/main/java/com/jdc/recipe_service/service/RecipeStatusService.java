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
    private final CommentService commentService;

    @Transactional(readOnly = true)
    public Map<Long, RecipeDetailStatusDto> getStatuses(List<Long> recipeIds, Long userId) {
        if (recipeIds == null || recipeIds.isEmpty()) {
            return Collections.emptyMap();
        }

        Map<Long, Long> recipeLikeCounts = recipeRepository.findLikeCountsMapByIds(recipeIds);

        Map<Long, List<CommentStatusDto>> commentStatusesMap = new HashMap<>();

        if (recipeIds.size() == 1) {
            Long recipeId = recipeIds.get(0);

            List<Long> commentIds = recipeCommentRepository.findTopNIdsByRecipeId(recipeId, Pageable.ofSize(3));

            if (!commentIds.isEmpty()) {
                List<CommentStatusDto> statuses = commentService.findCommentStatusesByCommentIds(userId, commentIds);
                commentStatusesMap.put(recipeId, statuses);
            }
        }

        if (userId == null) {
            Map<Long, RecipeDetailStatusDto> statusMap = new HashMap<>();
            for (Long recipeId : recipeIds) {
                RecipeDetailStatusDto status = RecipeDetailStatusDto.builder()
                        .likeCount(recipeLikeCounts.getOrDefault(recipeId, 0L).intValue())
                        .likedByCurrentUser(false)
                        .favoriteByCurrentUser(false)
                        .myRating(null)
                        .comments(commentStatusesMap.getOrDefault(recipeId, Collections.emptyList()))
                        .build();
                statusMap.put(recipeId, status);
            }
            return statusMap;
        }

        Set<Long> likedRecipeIds = recipeLikeRepository.findRecipeIdsByUserIdAndRecipeIdIn(userId, recipeIds);
        Set<Long> favoritedRecipeIds = recipeFavoriteRepository.findRecipeIdsByUserIdAndRecipeIdIn(userId, recipeIds);
        Map<Long, Integer> myRatings = recipeRatingRepository.findRatingsMapByUserIdAndRecipeIdIn(userId, recipeIds);

        Map<Long, RecipeDetailStatusDto> statusMap = new HashMap<>();
        for (Long recipeId : recipeIds) {

            Integer myRating = myRatings.getOrDefault(recipeId, null);

            RecipeDetailStatusDto status = RecipeDetailStatusDto.builder()
                    .likeCount(recipeLikeCounts.getOrDefault(recipeId, 0L).intValue())
                    .likedByCurrentUser(likedRecipeIds.contains(recipeId))
                    .favoriteByCurrentUser(favoritedRecipeIds.contains(recipeId))
                    .myRating(myRating)
                    .comments(commentStatusesMap.getOrDefault(recipeId, Collections.emptyList()))
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
                                .build()
                ));
    }
}