package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeStepIngredient;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
public interface RecipeStepIngredientRepository extends JpaRepository<RecipeStepIngredient, Long> {

    void deleteByRecipeIngredientId(Long recipeIngredientId);

    @Modifying
    @Transactional
    void deleteByStepId(Long id);
}
