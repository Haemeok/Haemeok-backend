package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.entity.RecipeStepIngredient;

import java.util.List;

public class StepIngredientMapper {

    public static RecipeStepIngredient toEntity(RecipeStepIngredientRequestDto dto, RecipeStep step, Ingredient ingredient) {
        return RecipeStepIngredient.builder()
                .step(step)
                .ingredient(ingredient)
                .quantity(dto.getQuantity())
                .unit(ingredient.getUnit())
                .build();
    }

    public static RecipeStepIngredientDto toDto(RecipeStepIngredient entity) {
        return RecipeStepIngredientDto.builder()
                .ingredientId(entity.getIngredient().getId())
                .name(entity.getIngredient().getName())
                .quantity(entity.getQuantity())
                .unit(entity.getUnit())
                .build();
    }

    public static List<RecipeStepIngredientDto> toDtoList(List<RecipeStepIngredient> entities) {
        return entities.stream().map(StepIngredientMapper::toDto).toList();
    }

}