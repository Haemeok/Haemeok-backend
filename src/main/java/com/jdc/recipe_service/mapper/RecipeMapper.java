package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeNutritionDto;
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
                .proteinG(Optional.ofNullable(nutrition.getProteinG()).orElse(zeroBigDecimal))
                .carbohydrateG(Optional.ofNullable(nutrition.getCarbohydrateG()).orElse(zeroBigDecimal))
                .fatG(Optional.ofNullable(nutrition.getFatG()).orElse(zeroBigDecimal))
                .sugarG(Optional.ofNullable(nutrition.getSugarG()).orElse(zeroBigDecimal))
                .sodiumMg(Optional.ofNullable(nutrition.getSodiumMg()).orElse(0))
                .build();
    }

}
