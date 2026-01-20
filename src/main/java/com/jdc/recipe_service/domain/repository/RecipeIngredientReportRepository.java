package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeIngredientReport;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface RecipeIngredientReportRepository extends JpaRepository<RecipeIngredientReport, Long> {
    List<RecipeIngredientReport> findByIsResolvedFalseOrderByCreatedAtDesc();

    List<RecipeIngredientReport> findByIngredientIdAndIsResolvedFalse(Long ingredientId);

    @Query("SELECT DISTINCT r.memberId FROM RecipeIngredientReport r WHERE r.recipeId = :recipeId AND r.isResolved = true")
    List<Long> findVerifiedMemberIds(@Param("recipeId") Long recipeId);
}