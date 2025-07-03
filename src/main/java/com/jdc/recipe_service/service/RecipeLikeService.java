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

    @Transactional
    public boolean toggleLike(Long userId, Long recipeId) {
        Optional<RecipeLike> like = likeRepository.findByUserIdAndRecipeId(userId, recipeId);

        if (like.isPresent()) {
            likeRepository.delete(like.get());
            return false;
        }

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        likeRepository.save(RecipeLike.builder().user(user).recipe(recipe).build());

        Long targetUserId = recipe.getUser().getId();
        if (!targetUserId.equals(userId)) {
            notificationService.createNotification(
                    NotificationCreateDto.builder()
                            .userId(targetUserId)
                            .actorId(userId)
                            .type(NotificationType.NEW_RECIPE_LIKE)
                            .content(user.getNickname() + "님이 레시피를 좋아합니다.")
                            .relatedType(NotificationRelatedType.RECIPE)
                            .relatedId(recipeId)
                            .relatedUrl("/recipes/" + recipeId)
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
