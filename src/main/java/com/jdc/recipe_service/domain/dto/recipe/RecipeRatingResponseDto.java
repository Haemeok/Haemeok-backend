package com.jdc.recipe_service.domain.dto.recipe;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class RecipeRatingResponseDto {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;
    private Double stars;
}