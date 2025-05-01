package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface RecipeQueryRepository {
    Page<RecipeSimpleDto> search(String title, DishType dishType, List<TagType> tagTypes, Pageable pageable, Long currentUserId);
    Page<RecipeSimpleDto> findAllSimpleWithRatingAndCookingInfo(Pageable pageable);
}
