package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface RecipeQueryRepositoryV2 {
    Page<RecipeSimpleStaticDto> searchStatic(String title, DishType dishType, List<TagType> tagTypes, Boolean isAiGenerated, Pageable pageable, Long currentUserId);
    Page<RecipeSimpleStaticDto> findAllSimpleStatic(Pageable pageable);
}
