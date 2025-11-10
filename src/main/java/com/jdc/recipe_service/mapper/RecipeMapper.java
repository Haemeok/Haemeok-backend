package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.DishType;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class RecipeMapper {

    public static Recipe toEntity(RecipeCreateRequestDto dto, User user) {
        Set<String> tools = dto.getCookingTools() != null
                ? new HashSet<>(dto.getCookingTools())
                : Collections.emptySet();

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
                .build();
    }

}
