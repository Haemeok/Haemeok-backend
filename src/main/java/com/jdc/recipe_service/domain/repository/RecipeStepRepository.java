package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeStep;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface RecipeStepRepository extends JpaRepository<RecipeStep, Long> {

    @EntityGraph(attributePaths = {"stepIngredients", "stepIngredients.ingredient"})
    List<RecipeStep> findByRecipeIdOrderByStepNumber(Long recipeId);

    @EntityGraph(attributePaths = {"stepIngredients", "stepIngredients.ingredient"})
    List<RecipeStep> findWithIngredientsByRecipeIdOrderByStepNumber(Long recipeId);

    @Modifying
    @Transactional
    void deleteByRecipeId(Long recipeId);
}
