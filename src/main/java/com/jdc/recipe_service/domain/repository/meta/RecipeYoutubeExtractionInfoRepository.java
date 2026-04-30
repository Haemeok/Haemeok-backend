package com.jdc.recipe_service.domain.repository.meta;

import com.jdc.recipe_service.domain.entity.media.RecipeYoutubeExtractionInfo;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface RecipeYoutubeExtractionInfoRepository
        extends JpaRepository<RecipeYoutubeExtractionInfo, Long> {

    Optional<RecipeYoutubeExtractionInfo> findByRecipeId(Long recipeId);
}
