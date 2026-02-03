package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.entity.RecipeStepIngredient;

import java.math.BigDecimal;
import java.util.List;

public class StepIngredientMapper {

    public static RecipeStepIngredient toEntity(RecipeStepIngredientRequestDto dto, RecipeStep step, RecipeIngredient recipeIngredient) {
        boolean isCustom = recipeIngredient.getIngredient() == null;

        String quantity = dto.getQuantity() != null && !dto.getQuantity().isBlank()
                ? dto.getQuantity()
                : recipeIngredient.getQuantity();

        String unit = isCustom
                ? dto.getCustomUnit()
                : recipeIngredient.getUnit();

        BigDecimal customPriceBd = null;
        if (isCustom && recipeIngredient.getCustomPrice() != null) {
            customPriceBd = BigDecimal.valueOf(recipeIngredient.getCustomPrice());
        }

        return RecipeStepIngredient.builder()
                .step(step)
                .recipeIngredient(recipeIngredient)
                .ingredient(isCustom ? null : recipeIngredient.getIngredient())
                .quantity(quantity)
                .unit(unit)
                .customName(isCustom && recipeIngredient.getCustomName() != null ? recipeIngredient.getCustomName().trim() : null)
                .customUnit(isCustom ? recipeIngredient.getCustomUnit() : null)
                .customPrice(customPriceBd)
                .build();
    }


    public static RecipeStepIngredientDto toDto(RecipeStepIngredient entity) {
        if (entity == null) return null;

        boolean isCustom = (entity.getIngredient() == null);

        Long ingredientId = (!isCustom && entity.getIngredient() != null)
                ? entity.getIngredient().getId()
                : null;

        String name = isCustom
                ? entity.getCustomName()
                : (entity.getIngredient() != null ? entity.getIngredient().getName() : null);

        return RecipeStepIngredientDto.builder()
                .ingredientId(ingredientId)
                .name(name)
                .quantity(entity.getQuantity())
                .unit(isCustom ? entity.getCustomUnit() : entity.getUnit())
                .build();
    }

    public static List<RecipeStepIngredientDto> toDtoList(List<RecipeStepIngredient> entities) {
        return entities.stream().map(StepIngredientMapper::toDto).toList();
    }
}
