package com.jdc.recipe_service.domain.dto.recipe;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepDto;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;


/**
 * 레시피 수정용
 */
@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeUpdateResponseDto {
    private Long id;
    private String title;
    private String dishType;
    private String description;
    private Integer cookingTime;
    private String imageUrl;
    private String youtubeUrl;
    private List<String> cookingTools;
   // private boolean isAiGenerated;

    private Integer servings;
    private Integer totalIngredientCost;
    private Integer marketPrice;

    private List<TagDto> tags;
    private List<RecipeIngredientDto> ingredients;
    private List<RecipeStepDto> steps;

//    private Boolean isPrivate;


}
