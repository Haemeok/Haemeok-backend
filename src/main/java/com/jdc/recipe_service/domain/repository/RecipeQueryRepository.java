package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import com.jdc.recipe_service.opensearch.dto.AiRecipeFilter;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface RecipeQueryRepository {
    Page<RecipeSimpleDto> search(String title, DishType dishType, List<TagType> tagTypes, AiRecipeFilter aiFilter,Integer maxCost, Pageable pageable, Long currentUserId);
    Page<RecipeSimpleDto> findAllSimpleWithRatingAndCookingInfo(Pageable pageable);
    Page<RecipeSimpleDto> searchAndSortByDynamicField(
            String title,
            DishType dishType,
            List<TagType> tags,
            AiRecipeFilter aiFilter,
            Integer maxCost,
            String property,
            org.springframework.data.domain.Sort.Direction direction,
            Pageable pageable,
            Long userId);

    Page<Recipe> findByFridgeFallback(
            List<Long> fridgeIds,
            AiRecipeFilter aiFilter,
            Pageable pageable
    );
}
