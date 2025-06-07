package com.jdc.recipe_service.domain.dto.recipe.step;

import lombok.*;


@Getter @Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeStepIngredientRequestDto {
    private String name;
    private String quantity;
    private String customUnit;

    public String getCustomName() {
        return name;
    }
}