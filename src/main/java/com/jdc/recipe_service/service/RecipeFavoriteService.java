package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeFavoriteRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Service
@RequiredArgsConstructor
public class RecipeFavoriteService {

    private final RecipeFavoriteRepository favoriteRepository;
    private final RecipeRepository recipeRepository;
    private final UserRepository userRepository;

    @Transactional
    public boolean toggleFavorite(Long userId, Long recipeId) {
        Optional<RecipeFavorite> favorite = favoriteRepository.findByUserIdAndRecipeId(userId, recipeId);

        if (favorite.isPresent()) {
            Recipe recipe = favorite.get().getRecipe();
            favoriteRepository.delete(favorite.get());

            recipe.decreaseFavoriteCount();
            return false;
        }

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        favoriteRepository.save(RecipeFavorite.builder().user(user).recipe(recipe).build());

        recipe.increaseFavoriteCount();
        return true;
    }

    /**
     * 즐겨찾기 강제 추가 (중복 체크 후)
     */
    @Transactional
    public void addFavoriteIfNotExists(Long userId, Long recipeId) {
        boolean exists = favoriteRepository.existsByRecipeIdAndUserId(recipeId, userId);

        if (exists) {
            return;
        }

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        favoriteRepository.save(RecipeFavorite.builder().user(user).recipe(recipe).build());

        recipe.increaseFavoriteCount();
    }

    @Transactional
    public void deleteByRecipeId(Long recipeId) {
        favoriteRepository.deleteByRecipeId(recipeId);
    }
}