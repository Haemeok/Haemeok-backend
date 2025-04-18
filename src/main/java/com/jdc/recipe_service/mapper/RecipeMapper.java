package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUserCreateRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.DishType;

import java.util.List;

public class RecipeMapper {

    public static Recipe toEntity(RecipeCreateRequestDto dto, User user) {
        return Recipe.builder()
                .user(user)
                .title(dto.getTitle())
                .description(dto.getDescription())
                .dishType(DishType.fromDisplayName(dto.getDishType()))
                .cookingTime(dto.getCookingTime())
                .imageKey(dto.getImageKey())
                .youtubeUrl(dto.getYoutubeUrl())
                .cookingTools(dto.getCookingTools())
                .servings(dto.getServings())
                .marketPrice(dto.getMarketPrice())
                .totalIngredientCost(dto.getTotalIngredientCost())
                .build();
    }

    public static Recipe toEntity(RecipeUserCreateRequestDto dto, User user) {
        return Recipe.builder()
                .user(user)
                .title(dto.getTitle())
                .description(dto.getDescription())
                .dishType(DishType.fromDisplayName(dto.getDishType()))
                .cookingTime(dto.getCookingTime())
                .imageKey(dto.getImageKey())
                .cookingTools(dto.getCookingTools())
                .servings(dto.getServings())
                .build();
    }

}
