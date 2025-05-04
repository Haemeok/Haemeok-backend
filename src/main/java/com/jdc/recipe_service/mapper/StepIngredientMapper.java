package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.entity.RecipeStepIngredient;

import java.util.List;

public class StepIngredientMapper {

    public static RecipeStepIngredient toEntity(RecipeStepIngredientRequestDto dto, RecipeStep step, Ingredient ingredient) {
        boolean isCustom = (ingredient == null);

        return RecipeStepIngredient.builder()
                .step(step)
                .ingredient(ingredient)
                .quantity(dto.getQuantity())
                .unit(isCustom ? dto.getCustomUnit() : ingredient.getUnit())
                .customName(isCustom ? dto.getName() : null)
                .customUnit(isCustom ? dto.getCustomUnit() : null)
                .build();
    }

    public static RecipeStepIngredientDto toDto(RecipeStepIngredient entity) {
        boolean isCustom = (entity.getIngredient() == null);

        return RecipeStepIngredientDto.builder()
                .ingredientId(isCustom ? null : entity.getIngredient().getId())
                .name(isCustom ? entity.getCustomName() : entity.getIngredient().getName())
                .quantity(entity.getQuantity())
                .unit(isCustom ? entity.getCustomUnit() : entity.getUnit())
                .build();
    }

    public static List<RecipeStepIngredientDto> toDtoList(List<RecipeStepIngredient> entities) {
        return entities.stream().map(StepIngredientMapper::toDto).toList();
    }
}