package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.notification.NotificationCreateDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeLike;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class RecipeLikeService {


    private final RecipeLikeRepository likeRepository;
    private final RecipeRepository recipeRepository;
    private final UserRepository userRepository;
    private final NotificationService notificationService;
    private final Hashids hashids;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    public String generateImageUrl(String key) {
        return key == null ? null :
                String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    @Transactional
    public boolean toggleLike(Long userId, Long recipeId) {
        Optional<RecipeLike> like = likeRepository.findByUserIdAndRecipeId(userId, recipeId);

        if (like.isPresent()) {
            likeRepository.delete(like.get());

            Recipe recipe = like.get().getRecipe();
            recipe.decreaseLikeCount();

            return false;
        }

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        likeRepository.save(RecipeLike.builder().user(user).recipe(recipe).build());
        recipe.increaseLikeCount();

        Long targetUserId = recipe.getUser().getId();
        if (!targetUserId.equals(userId)) {
            String encodedId = hashids.encode(recipeId);
            notificationService.createNotification(
                    NotificationCreateDto.builder()
                            .userId(targetUserId)
                            .actorId(userId)
                            .actorNickname(user.getNickname())
                            .imageUrl(generateImageUrl(recipe.getImageKey()))
                            .type(NotificationType.NEW_RECIPE_LIKE)
                            .relatedType(NotificationRelatedType.RECIPE)
                            .relatedId(recipeId)
                            .relatedUrl("/recipes/" + encodedId)
                            .build()
            );
        }
        return true;
    }

    @Transactional
    public void deleteByRecipeId(Long recipeId) {
        likeRepository.deleteByRecipeId(recipeId);
    }


}
