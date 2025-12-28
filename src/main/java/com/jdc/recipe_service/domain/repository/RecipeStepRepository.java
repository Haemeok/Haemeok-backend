package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeStep;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface RecipeStepRepository extends JpaRepository<RecipeStep, Long> {

    @Query("SELECT rs FROM RecipeStep rs WHERE rs.recipe.id = :recipeId ORDER BY rs.stepNumber")
    List<RecipeStep> findByRecipeIdOrderByStepNumber(@Param("recipeId") Long recipeId);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeStep rs WHERE rs.recipe.id = :recipeId")
    @Transactional
    void deleteByRecipeId(@Param("recipeId") Long recipeId);
}
