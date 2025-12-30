package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.opensearch.dto.AiRecipeFilter;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;

import java.time.LocalDateTime;
import java.util.List;

public interface RecipeQueryRepository {
    Page<RecipeSimpleDto> search(RecipeSearchCondition condition, Pageable pageable, Long currentUserId);

    Page<RecipeSimpleDto> findPopularRecipesSince(LocalDateTime startDate, Pageable pageable);

    Page<RecipeSimpleDto> findBudgetRecipes(Integer maxCost, Pageable pageable);

    Page<RecipeSimpleDto> searchAndSortByDynamicField(
            RecipeSearchCondition condition,
            String property,
            Sort.Direction direction,
            Pageable pageable,
            Long userId);

    Page<Recipe> findByFridgeFallback(
            List<Long> fridgeIds,
            AiRecipeFilter aiFilter,
            Pageable pageable
    );
}
