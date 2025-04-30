package com.jdc.recipe_service.domain.dto.fridge;


public record RefrigeratorItemSummaryDto(
        Long id,
        String name,
        String category,
        String imageUrl
) { }