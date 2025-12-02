package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.user.ReactionRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeLike;
import com.jdc.recipe_service.domain.entity.RecipeRating;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
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
import java.util.Random;

@Service
@RequiredArgsConstructor
public class ReactionService {

    private final RecipeRepository recipeRepository;
    private final UserRepository userRepository;
    private final RecipeLikeRepository recipeLikeRepository;
    private final RecipeRatingRepository recipeRatingRepository;

    private static final Long TEST_USER_START_ID = 90001L;
    private static final int MAX_TEST_USERS = 100;

    @Transactional
    public void addReactions(Long recipeId, ReactionRequestDto dto) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        if (dto.getLikeCount() > MAX_TEST_USERS || dto.getRatingCount() > MAX_TEST_USERS) {
            throw new IllegalArgumentException("테스트 유저 수는 최대 100명까지만 가능합니다.");
        }

        Random random = new Random();

        for (int i = 0; i < dto.getLikeCount(); i++) {
            Long userId = TEST_USER_START_ID + i;
            User user = getUserOrThrow(userId);

            if (!recipeLikeRepository.existsByUserAndRecipe(user, recipe)) {
                recipeLikeRepository.save(RecipeLike.builder()
                        .user(user)
                        .recipe(recipe)
                        .build());
                recipe.increaseLikeCount();
            }
        }

        double baseRating = 3.8 + (random.nextInt(8) * 0.1);

        for (int i = 0; i < dto.getRatingCount(); i++) {
            Long userId = TEST_USER_START_ID + i;
            User user = getUserOrThrow(userId);

            double tempScore = baseRating + ((random.nextInt(11) - 5) * 0.1);
            tempScore = Math.round(tempScore * 10.0) / 10.0;

            if (tempScore > 5.0) tempScore = 5.0;
            if (tempScore < 0.0) tempScore = 0.0;

            double finalScore = tempScore;

            RecipeRating rating = recipeRatingRepository.findByUserAndRecipe(user, recipe)
                    .orElseGet(() -> RecipeRating.builder()
                            .user(user)
                            .recipe(recipe)
                            .rating(finalScore)
                            .build());

            rating.updateRating(finalScore);
            recipeRatingRepository.save(rating);
        }

        updateRecipeStats(recipe);
    }

    private User getUserOrThrow(Long userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
    }

    private void updateRecipeStats(Recipe recipe) {
        Double avg = recipeRatingRepository.calculateAverageByRecipeId(recipe.getId());
        long count = recipeRatingRepository.countByRecipeId(recipe.getId());

        double avgValue = (avg != null) ? avg : 0.0;

        recipe.updateAvgRating(BigDecimal.valueOf(avgValue).setScale(2, RoundingMode.HALF_UP));
        recipe.updateRatingCount(count);
    }
}