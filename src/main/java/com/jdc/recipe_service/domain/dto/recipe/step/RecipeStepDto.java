package com.jdc.recipe_service.domain.dto.recipe.step;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeStepDto {
    private int stepNumber;
    private String instruction;
    private String stepImageUrl;
    private String stepImageKey;
    private String action;

    private List<RecipeStepIngredientDto> ingredients;
}
