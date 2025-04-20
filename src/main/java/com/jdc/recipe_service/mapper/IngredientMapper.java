package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.ingredient.*;
import com.jdc.recipe_service.domain.entity.Ingredient;

public class IngredientMapper {

    public static Ingredient toEntity(IngredientRequestDto dto) {
        return Ingredient.builder()
                .name(dto.getName())
                .category(dto.getCategory())
                .imageUrl(dto.getImageUrl())
                .price(dto.getPrice())
                .unit(dto.getUnit())
                .build();
    }

    public static IngredientResponseDto toDto(Ingredient entity) {
        return IngredientResponseDto.builder()
                .id(entity.getId())
                .name(entity.getName())
                .category(entity.getCategory())
                .imageUrl(entity.getImageUrl())
                .price(entity.getPrice())
                .unit(entity.getUnit())
                .build();
    }
}
