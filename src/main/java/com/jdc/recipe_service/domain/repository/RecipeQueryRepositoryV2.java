package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;


public interface RecipeQueryRepositoryV2 {
    Page<RecipeSimpleStaticDto> searchStatic(RecipeSearchCondition condition, Pageable pageable, Long currentUserId);
}
