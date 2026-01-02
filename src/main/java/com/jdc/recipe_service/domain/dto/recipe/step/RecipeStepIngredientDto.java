package com.jdc.recipe_service.domain.dto.recipe.step;


import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeStepIngredientDto {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long ingredientId;
    private String name;
    private String quantity;
    private String unit;
}
