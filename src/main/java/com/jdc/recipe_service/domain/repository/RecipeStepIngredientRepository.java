package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeStepIngredient;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface RecipeStepIngredientRepository extends JpaRepository<RecipeStepIngredient, Long> {

    void deleteByRecipeIngredientId(Long recipeIngredientId);

    @Modifying
    @Transactional
    void deleteByStepId(Long id);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeStepIngredient rsi WHERE rsi.step.id IN (SELECT s.id FROM RecipeStep s WHERE s.recipe.id = :recipeId)")
    void deleteByRecipeId(@Param("recipeId") Long recipeId);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeStepIngredient rsi WHERE rsi.step.id IN (SELECT s.id FROM RecipeStep s WHERE s.recipe.id = :recipeId)")
    void deleteAllByRecipeId(@Param("recipeId") Long recipeId);

    @Query("SELECT rsi FROM RecipeStepIngredient rsi LEFT JOIN FETCH rsi.ingredient WHERE rsi.step.id IN :stepIds")
    List<RecipeStepIngredient> findByStepIdIn(@Param("stepIds") List<Long> stepIds);
}
