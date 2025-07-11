package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.notification.NotificationCreateDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingResponseDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeComment;
import com.jdc.recipe_service.domain.entity.RecipeRating;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeCommentRepository;
import com.jdc.recipe_service.domain.repository.RecipeRatingRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
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
    private final CookingRecordService cookingRecordService;
    private final NotificationService notificationService;

    @Transactional
    public RecipeRatingResponseDto rateRecipe(Long recipeId, Long userId, RecipeRatingRequestDto dto) {
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

        RecipeRating saved = ratingRepository.save(rating);

        updateRecipeAverageRating(recipe);

        if (dto.getComment() != null && !dto.getComment().isBlank()) {
            RecipeComment comment = RecipeComment.builder()
                    .user(user)
                    .recipe(recipe)
                    .comment(dto.getComment())
                    .build();

            recipeCommentRepository.save(comment);
            Long targetUserId = recipe.getUser().getId();
            if (!targetUserId.equals(userId)) {
                notificationService.createNotification(
                        NotificationCreateDto.builder()
                                .userId(targetUserId)
                                .actorId(userId)
                                .type(NotificationType.NEW_RECIPE_RATING)
                                .content(user.getNickname() + "님이 레시피에 평점과 함께 댓글을 남겼습니다.")
                                .relatedType(NotificationRelatedType.RECIPE)
                                .relatedId(recipeId)
                                .relatedUrl("/recipes/" + recipeId + "/comments")
                                .build()
                );
            }
        }

        cookingRecordService.createRecordFromRating(
                userId, recipeId, saved.getId()
        );

        return new RecipeRatingResponseDto(
                saved.getId(),
                saved.getRating()
        );
    }

    @Transactional
    public Long deleteRating(Long recipeId, Long userId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        RecipeRating rating = ratingRepository.findByUserAndRecipe(user, recipe)
                .orElseThrow(() -> new CustomException(ErrorCode.RATING_NOT_FOUND));

        Long deletedId = rating.getId();
        ratingRepository.delete(rating);

        updateRecipeAverageRating(recipe);


        return deletedId;
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
        long count = ratingRepository.countByRecipeId(recipe.getId());

        recipe.updateAvgRating(BigDecimal.valueOf(avg).setScale(2, RoundingMode.HALF_UP));
        recipe.updateRatingCount(count);
    }
}
