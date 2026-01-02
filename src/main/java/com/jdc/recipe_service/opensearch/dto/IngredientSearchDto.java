package com.jdc.recipe_service.opensearch.dto;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;

public record IngredientSearchDto(
        @JsonSerialize(using = HashIdSerializer.class)
        Long id,
        String name,
        String category,
        String imageUrl,
        String unit
) { }
