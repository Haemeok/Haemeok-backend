package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeImage;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.List;

public interface RecipeImageRepository extends JpaRepository<RecipeImage, Long> {
    List<RecipeImage> findByRecipeId(Long recipeId);
}
