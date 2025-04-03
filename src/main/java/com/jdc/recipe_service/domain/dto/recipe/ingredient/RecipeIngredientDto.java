package com.jdc.recipe_service.domain.dto.recipe.ingredient;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;


/**
 *
 *  재료 응답용
 */


@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeIngredientDto {
    private Long ingredientId;
    private String name;
    private String quantity;
    private String unit;
    private Integer price;
}
