package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import org.springframework.data.domain.Pageable;
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

    @Query("SELECT ri.ingredient.id, COUNT(ri) " +
            "FROM RecipeIngredient ri " +
            "WHERE ri.ingredient IS NOT NULL " +
            "GROUP BY ri.ingredient.id")
    List<Object[]> countIngredientsUsage();

    @Query("SELECT ri FROM RecipeIngredient ri " +
            "WHERE ri.ingredient IS NULL " +
            "AND ri.customLink IS NULL")
    List<RecipeIngredient> findCustomIngredientsNeedLink(Pageable pageable);

    @Query(value = "SELECT DISTINCT r.custom_name FROM recipe_ingredients r " +
            "WHERE r.ingredient_id IS NULL " +
            "AND r.custom_link IS NULL " +
            "AND r.custom_name IS NOT NULL " +
            "AND r.custom_name != '' " +
            "LIMIT 30", nativeQuery = true)
    List<String> findDistinctNamesNeedLink();

    @Modifying(clearAutomatically = true)
    @Transactional
    @Query("UPDATE RecipeIngredient ri " +
            "SET ri.customLink = :link " +
            "WHERE ri.customName = :name " +
            "AND ri.customLink IS NULL " +
            "AND ri.ingredient IS NULL")
    int updateLinkByCustomName(@Param("name") String name, @Param("link") String link);
}