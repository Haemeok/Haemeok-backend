package com.jdc.recipe_service.opensearch.dto;

public record IngredientSearchDto(
        Long id,
        String name,
        String category,
        String imageUrl
) { }
