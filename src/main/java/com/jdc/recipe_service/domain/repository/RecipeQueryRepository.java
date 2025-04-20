package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface RecipeQueryRepository {
    Page<RecipeSimpleDto> search(RecipeSearchCondition condition, Pageable pageable, Long currentUserId);
    Page<RecipeSimpleDto> findAllSimpleWithRatingAndCookingInfo(Pageable pageable);
}
