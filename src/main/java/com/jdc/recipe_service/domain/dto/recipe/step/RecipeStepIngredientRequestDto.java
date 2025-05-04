package com.jdc.recipe_service.domain.dto.recipe.step;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeStepIngredientRequestDto {
    private String name;
    private String quantity;

    private String customUnit;
}