package com.jdc.recipe_service.domain.dto.recipe.step;

import lombok.*;

import java.util.List;

@Getter @Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeStepRequestDto {
    private int stepNumber;
    private String instruction;
    private String imageKey;
    private String action;

    private List<RecipeStepIngredientRequestDto> ingredients;

    public void updateImageKey(String imageKey) {
        this.imageKey = imageKey;
    }
}
