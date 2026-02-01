package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeNutritionDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.DishType;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

public class RecipeMapper {

    public static Recipe toEntity(RecipeCreateRequestDto dto, User user) {
        Set<String> tools = dto.getCookingTools() != null
                ? new HashSet<>(dto.getCookingTools())
                : Collections.emptySet();

        RecipeNutritionDto nutrition = dto.getNutrition() != null
                ? dto.getNutrition()
                : new RecipeNutritionDto();

        BigDecimal zeroBigDecimal = BigDecimal.valueOf(0.00);

        return Recipe.builder()
                .user(user)
                .title(dto.getTitle())
                .description(dto.getDescription())
                .cookingTips(dto.getCookingTips())
                .dishType(DishType.fromDisplayName(dto.getDishType()))
                .cookingTime(dto.getCookingTime())
                .imageKey(dto.getImageKey())
                .youtubeUrl(dto.getYoutubeUrl())
                .cookingTools(tools)
                .servings(dto.getServings())
                .marketPrice(dto.getMarketPrice())
                .totalIngredientCost(0)
                .isPrivate(dto.getIsPrivate() != null ? dto.getIsPrivate() : false)
                .protein(Optional.ofNullable(nutrition.getProtein()).orElse(zeroBigDecimal))
                .carbohydrate(Optional.ofNullable(nutrition.getCarbohydrate()).orElse(zeroBigDecimal))
                .fat(Optional.ofNullable(nutrition.getFat()).orElse(zeroBigDecimal))
                .sugar(Optional.ofNullable(nutrition.getSugar()).orElse(zeroBigDecimal))
                .sodium(Optional.ofNullable(nutrition.getSodium()).orElse(BigDecimal.ZERO))
                .totalCalories(zeroBigDecimal)
                .build();
    }

    public static RecipeCreateRequestDto toCreateDto(Recipe recipe) {
        return RecipeCreateRequestDto.builder()
                .title(recipe.getTitle())
                .description(recipe.getDescription())
                .cookingTips(recipe.getCookingTips())
                .dishType(recipe.getDishType().getDisplayName())
                .cookingTime(recipe.getCookingTime())
                .servings(recipe.getServings())
                .ingredients(recipe.getIngredients().stream()
                        .map(ri -> RecipeIngredientRequestDto.builder()
                                .name(ri.getCustomName())
                                .quantity(ri.getQuantity())
                                .customUnit(ri.getCustomUnit())
                                .build())
                        .toList())
                .steps(recipe.getSteps().stream()
                        .map(rs -> RecipeStepRequestDto.builder()
                                .stepNumber(rs.getStepNumber())
                                .instruction(rs.getInstruction())
                                .action(rs.getAction())
                                .timeline(rs.getTimeline())
                                .build())
                        .toList())
                .build();
    }

}
