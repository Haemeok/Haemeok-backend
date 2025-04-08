package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.DishType;

public class RecipeMapper {

    public static Recipe toEntity(RecipeCreateRequestDto dto, User user) {
        return Recipe.builder()
                .user(user)
                .title(dto.getTitle())
                .description(dto.getDescription())
                .dishType(DishType.fromDisplayName(dto.getDishType())) // displayName → Enum 변환
                .cookingTime(dto.getCookingTime())
                .imageUrl(dto.getImageUrl())
                .youtubeUrl(dto.getYoutubeUrl())
                .cookingTools(dto.getCookingTools())
                .servings(dto.getServings())
                .marketPrice(dto.getMarketPrice())
                .totalIngredientCost(dto.getTotalIngredientCost())
                .build();
    }



}
