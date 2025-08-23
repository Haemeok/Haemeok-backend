package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.recipe.v2.RecipeStatusDto;
import com.jdc.recipe_service.domain.repository.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
    public Map<Long, RecipeStatusDto> getStatuses(List<Long> recipeIds, Long userId) {
        if (recipeIds == null || recipeIds.isEmpty()) {
            return Collections.emptyMap();
        }

        Set<Long> likedIds = (userId == null) ? Collections.emptySet() :
                recipeLikeRepository.findRecipeIdsByUserIdAndRecipeIdIn(userId, recipeIds);
        Set<Long> favoritedIds = (userId == null) ? Collections.emptySet() :
                recipeFavoriteRepository.findRecipeIdsByUserIdAndRecipeIdIn(userId, recipeIds);
        Map<Long, Integer> myRatings = (userId == null) ? Collections.emptyMap() :
                recipeRatingRepository.findRatingsMapByUserIdAndRecipeIdIn(userId, recipeIds);

        Map<Long, Long> likeCounts = recipeRepository.findLikeCountsMapByIds(recipeIds);
        Map<Long, Long> commentCounts = recipeRepository.findCommentCountsMapByIds(recipeIds);
        Map<Long, Double> avgRatings = recipeRepository.findAvgRatingsMapByIds(recipeIds);
        Map<Long, Long> ratingCounts = recipeRepository.findRatingCountsMapByIds(recipeIds);

        Map<Long, RecipeStatusDto> statusMap = new HashMap<>();
        for (Long recipeId : recipeIds) {
            List<CommentDto> comments = null;
            if (recipeIds.size() == 1) {
                comments = commentService.getTop3CommentsWithLikes(recipeId, userId);
            }

            RecipeStatusDto status = RecipeStatusDto.builder()
                    .likeCount(likeCounts.getOrDefault(recipeId, 0L))
                    .commentCount(commentCounts.getOrDefault(recipeId, 0L))
                    .avgRating(avgRatings.getOrDefault(recipeId, 0.0))
                    .ratingCount(ratingCounts.getOrDefault(recipeId, 0L))
                    .likedByCurrentUser(likedIds.contains(recipeId))
                    .favoriteByCurrentUser(favoritedIds.contains(recipeId))
                    .myRating(myRatings.get(recipeId))
                    .comments(comments)
                    .build();
            statusMap.put(recipeId, status);
        }
        return statusMap;
    }
}