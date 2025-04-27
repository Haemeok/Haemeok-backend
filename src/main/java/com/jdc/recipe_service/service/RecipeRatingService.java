package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeComment;
import com.jdc.recipe_service.domain.entity.RecipeRating;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeCommentRepository;
import com.jdc.recipe_service.domain.repository.RecipeRatingRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;

@Service
@RequiredArgsConstructor
public class RecipeRatingService {

    private final RecipeRatingRepository ratingRepository;
    private final RecipeRepository recipeRepository;
    private final UserRepository userRepository;
    private final RecipeCommentRepository recipeCommentRepository;

    @Transactional
    public void rateRecipe(Long recipeId, Long userId, RecipeRatingRequestDto dto) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        RecipeRating rating = ratingRepository.findByUserAndRecipe(user, recipe)
                .map(existing -> {
                    existing.updateRating(dto.getRating());
                    return existing;
                })
                .orElseGet(() -> RecipeRating.builder()
                        .user(user)
                        .recipe(recipe)
                        .rating(dto.getRating())
                        .build()
                );

        ratingRepository.save(rating);

        updateRecipeAverageRating(recipe);

        // ✨ 코멘트가 있다면 댓글로 저장
        if (dto.getComment() != null && !dto.getComment().isBlank()) {
            RecipeComment comment = RecipeComment.builder()
                    .user(user)
                    .recipe(recipe)
                    .comment(dto.getComment())
                    .isDeleted(false)
                    .build();

            recipeCommentRepository.save(comment);
        }
    }

    @Transactional
    public void deleteRating(Long recipeId, Long userId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        RecipeRating rating = ratingRepository.findByUserAndRecipe(user, recipe)
                .orElseThrow(() -> new CustomException(ErrorCode.RATING_NOT_FOUND));

        ratingRepository.delete(rating);

        updateRecipeAverageRating(recipe);
    }

    public Double getMyRating(Long recipeId, Long userId) {
        return ratingRepository.findByUserIdAndRecipeId(userId, recipeId)
                .map(RecipeRating::getRating)
                .orElse(0.0);
    }

    public long getRatingCount(Long recipeId) {
        return ratingRepository.countByRecipeId(recipeId);
    }

    private void updateRecipeAverageRating(Recipe recipe) {
        double avg = ratingRepository.calculateAverageByRecipeId(recipe.getId());
        recipe.setAvgRating(BigDecimal.valueOf(avg).setScale(2, RoundingMode.HALF_UP));
        recipeRepository.save(recipe);
    }
}
