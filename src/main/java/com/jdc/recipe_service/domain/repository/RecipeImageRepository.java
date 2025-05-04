package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeImage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

public interface RecipeImageRepository extends JpaRepository<RecipeImage, Long> {
    List<RecipeImage> findByRecipeId(Long recipeId);

    @Transactional
    @Modifying
    @Query("DELETE FROM RecipeImage i WHERE i.recipe.id = :recipeId")
    void deleteByRecipeId(Long recipeId);
}
