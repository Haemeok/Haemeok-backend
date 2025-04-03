package com.jdc.recipe_service.domain.dto.recipe.step;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeStepIngredientDto {
    private Long ingredientId;
    private String name;
    private String quantity;
    private String unit;
}
