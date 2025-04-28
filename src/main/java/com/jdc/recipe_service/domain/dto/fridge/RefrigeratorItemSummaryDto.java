package com.jdc.recipe_service.domain.dto.fridge;

import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;

public record RefrigeratorItemSummaryDto(
        Long               id,
        IngredientSummaryDto ingredient
) {}