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
import org.springframework.beans.factory.annotation.Value;
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
    private final NotificationService notificationService;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    public String generateImageUrl(String key) {
        return key == null ? null :
                String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }
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

        ratingRepository.save(rating);

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
                                .actorNickname(user.getNickname())
                                .imageUrl(generateImageUrl(recipe.getImageKey()))
                                .type(NotificationType.NEW_RECIPE_RATING)
                                .relatedType(NotificationRelatedType.RECIPE)
                                .relatedId(recipeId)
                                .relatedUrl("/recipes/" + recipeId + "/comments")
                                .build()
                );
            }
        }

        return new RecipeRatingResponseDto(
                rating.getId(),
                rating.getRating()
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
