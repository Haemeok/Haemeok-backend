package com.jdc.recipe_service.domain.dto.ingredient;

public record IngredientSummaryDto(
        Long    id,
        String  name,
        String  category,
        String  imageUrl,
        boolean inFridge
) { }
