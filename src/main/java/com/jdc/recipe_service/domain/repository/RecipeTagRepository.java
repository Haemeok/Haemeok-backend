package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeTag;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface RecipeTagRepository extends JpaRepository<RecipeTag, Long> {

    @EntityGraph(attributePaths = {"tag"})
    List<RecipeTag> findByRecipeId(Long recipeId);

    @EntityGraph(attributePaths = {"tag"})
    List<RecipeTag> findByRecipeIdIn(@Param("recipeIds") List<Long> recipeIds);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeTag rt WHERE rt.recipe.id = :recipeId")
    @Transactional
    void deleteByRecipeId(@Param("recipeId") Long recipeId);
}