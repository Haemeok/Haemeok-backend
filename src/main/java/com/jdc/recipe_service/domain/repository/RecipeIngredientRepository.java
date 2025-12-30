package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface RecipeIngredientRepository extends JpaRepository<RecipeIngredient, Long> {
    @EntityGraph(attributePaths = {"ingredient"})
    List<RecipeIngredient> findByRecipeId(Long recipeId);

    @EntityGraph(attributePaths = {"ingredient"})
    List<RecipeIngredient> findByRecipeIdIn(List<Long> recipeIds);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeIngredient ri WHERE ri.recipe.id = :recipeId")
    @Transactional
    void deleteByRecipeId(@Param("recipeId") Long recipeId);
}