package com.jdc.recipe_service.domain.dto.recipe;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import jakarta.validation.constraints.NotBlank;
import lombok.*;

import java.util.List;

/**
 * 레시피 생성용
 */

@Getter @Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeCreateRequestDto {

    private Boolean isRecipe;
    private String nonRecipeReason;

    @NotBlank(message = "레시피 제목은 필수입니다.")
    private String title;

    private String description;

    private String cookingTips;

    @NotBlank(message = "요리 유형은 필수입니다.")
    private String dishType;

    private Integer cookingTime;
    private String imageKey;
    private String youtubeUrl;
    private List<String> cookingTools;

    private Integer servings;
    private Integer totalIngredientCost;
    private Integer marketPrice;
    private Boolean isPrivate;
    private Double totalCalories;

    private RecipeNutritionDto nutrition;

    private List<RecipeIngredientRequestDto> ingredients;
    private List<RecipeStepRequestDto> steps;
    private List<String> tags;

    private List<ComponentResponseDto> components;
    private PlatingResponseDto plating;

    @Getter @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ComponentResponseDto {
        private String role;
        private String name;
        private String description;
        private List<String> process;
    }

    @Getter @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PlatingResponseDto {
        private String vessel;
        private String guide;
        private List<String> visualKeys;
        private String viewpoint;
        private String lighting;
    }
}
