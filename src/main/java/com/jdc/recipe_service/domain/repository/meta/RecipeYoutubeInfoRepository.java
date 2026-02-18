package com.jdc.recipe_service.domain.repository.meta;

import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeInfo;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RecipeYoutubeInfoRepository extends JpaRepository<RecipeYoutubeInfo, Long> {
    boolean existsByRecipeId(Long recipeId);
}