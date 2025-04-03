package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeStep;

import java.util.List;

public class RecipeStepMapper {

    public static RecipeStep toEntity(RecipeStepRequestDto dto, Recipe recipe) {
        return RecipeStep.builder()
                .recipe(recipe)
                .stepNumber(dto.getStepNumber())
                .instruction(dto.getInstruction())
                .stepImageUrl(dto.getStepImageUrl())
                .cookingTools(dto.getCookingTools())
                .action(dto.getAction())
                .build();
    }

    public static RecipeStepDto toDto(RecipeStep step, List<RecipeStepIngredientDto> ingredients) {
        return RecipeStepDto.builder()
                .stepNumber(step.getStepNumber())
                .instruction(step.getInstruction())
                .stepImageUrl(step.getStepImageUrl())
                .cookingTools(step.getCookingTools())
                .action(step.getAction())
                .ingredients(ingredients)
                .build();
    }
}

