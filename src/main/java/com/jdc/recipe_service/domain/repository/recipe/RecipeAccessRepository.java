package com.jdc.recipe_service.domain.repository.recipe;

import com.jdc.recipe_service.domain.entity.recipe.RecipeAccess;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RecipeAccessRepository extends JpaRepository<RecipeAccess, Long> {
    boolean existsByUserIdAndRecipeId(Long userId, Long recipeId);
}