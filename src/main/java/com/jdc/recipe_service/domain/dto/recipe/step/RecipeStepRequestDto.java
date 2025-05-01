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
public class RecipeStepRequestDto {
    private int stepNumber;
    private String instruction;
    private String imageKey; // 🔄 imageUrl → imageKey (S3 key 저장용)
    private List<String> stepImageKeys;
    private String action;

    private List<RecipeStepIngredientRequestDto> ingredients;

    public void setImageKey(String imageKey) {
        this.imageKey = imageKey;
    }
}
