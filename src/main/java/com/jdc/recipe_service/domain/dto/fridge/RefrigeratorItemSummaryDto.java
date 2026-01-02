package com.jdc.recipe_service.domain.dto.fridge;


import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;

public record RefrigeratorItemSummaryDto(
        @JsonSerialize(using = HashIdSerializer.class)
        Long id,
        String name,
        String category,
        String imageUrl,
        String unit
) { }