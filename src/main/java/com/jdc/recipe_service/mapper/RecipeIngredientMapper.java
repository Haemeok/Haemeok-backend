package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;


import java.util.List;

public class RecipeIngredientMapper {

    public static RecipeIngredient toEntity(RecipeIngredientRequestDto dto, Recipe recipe, Ingredient ingredient) {
        return RecipeIngredient.builder()
                .recipe(recipe)
                .ingredient(ingredient)
                .quantity(dto.getQuantity())
                .unit(dto.getUnit())
                .build();
    }

    public static RecipeIngredientDto toDto(RecipeIngredient entity) {
        return RecipeIngredientDto.builder()
                .ingredientId(entity.getIngredient().getId())
                .name(entity.getIngredient().getName())
                .quantity(entity.getQuantity())
                .unit(entity.getUnit())
                .price(entity.getIngredient().getPrice())
                .build();
    }

    public static List<RecipeIngredientDto> toDtoList(List<RecipeIngredient> entities) {
        return entities.stream().map(RecipeIngredientMapper::toDto).toList();
    }
}
