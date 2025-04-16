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
                .unit(ingredient.getUnit())
                .build();
    }

    public static RecipeIngredientDto toDto(RecipeIngredient entity) {
        int unitPrice = entity.getIngredient().getPrice() != null ? entity.getIngredient().getPrice() : 0;
        int quantity = Integer.parseInt(entity.getQuantity());
        int totalPrice = unitPrice * quantity;

        return RecipeIngredientDto.builder()
                .ingredientId(entity.getIngredient().getId())
                .name(entity.getIngredient().getName())
                .quantity(entity.getQuantity())
                .unit(entity.getUnit())
                .price(totalPrice)
                .build();
    }

    public static List<RecipeIngredientDto> toDtoList(List<RecipeIngredient> entities) {
        return entities.stream().map(RecipeIngredientMapper::toDto).toList();
    }
}
