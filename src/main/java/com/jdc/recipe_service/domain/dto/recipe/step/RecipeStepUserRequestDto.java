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
public class RecipeStepUserRequestDto {
    private int stepNumber;
    private String instruction;
    private String imageKey;
    private List<String> stepImageKeys;

    private List<RecipeStepIngredientRequestDto> ingredients;
}
