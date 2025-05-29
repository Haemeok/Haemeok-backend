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
            favoriteRepository.delete(favorite.get());
            return false;
        }

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        favoriteRepository.save(RecipeFavorite.builder().user(user).recipe(recipe).build());
        return true;
    }

    @Transactional
    public void deleteByRecipeId(Long recipeId) {
        favoriteRepository.deleteByRecipeId(recipeId);
    }

}
