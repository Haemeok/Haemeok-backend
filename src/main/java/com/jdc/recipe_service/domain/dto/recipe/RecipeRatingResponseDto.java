package com.jdc.recipe_service.domain.dto.recipe;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class RecipeRatingResponseDto {
    private Long id;
    private Double stars;
}