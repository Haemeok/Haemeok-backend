package com.jdc.recipe_service.domain.dto.recipe;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import lombok.*;

import java.util.List;

/**
 * 레시피 수정용
 */
@Getter @Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeUpdateRequestDto {

    private String title;
    private String description;
    private String cookingTips;
    private String dishType;
    private Integer cookingTime;
    private String imageKey;
    private String youtubeUrl;
    private List<String> cookingTools;

    private Integer servings;
    private Integer totalIngredientCost;
    private Integer marketPrice;
    private Boolean isPrivate;

    private RecipeNutritionDto nutrition;

    private List<RecipeIngredientRequestDto> ingredients;
    private List<RecipeStepRequestDto> steps;
    private List<String> tags;

    @Builder.Default
    private Boolean isIngredientsModified = false;
}