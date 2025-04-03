package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeTag;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface RecipeTagRepository extends JpaRepository<RecipeTag, Long> {

    @EntityGraph(attributePaths = {"tag"})
    List<RecipeTag> findByRecipeId(Long recipeId);

    @Modifying
    @Transactional
    void deleteByRecipeId(Long recipeId);
}