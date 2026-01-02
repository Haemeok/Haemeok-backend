package com.jdc.recipe_service.domain.dto.calendar;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.Getter;

import java.math.BigDecimal;

@Getter
public class CookingRecordSummaryDto {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long recordId;
    @JsonSerialize(using = HashIdSerializer.class)
    private Long recipeId;
    private String recipeTitle;
    private Integer ingredientCost;
    private Integer marketPrice;
    private NutritionSummaryDto nutrition;
    private BigDecimal calories;
    private String imageUrl;

    public static CookingRecordSummaryDto from(
            com.jdc.recipe_service.domain.entity.CookingRecord e,
            String imageUrl
    ) {
        var dto = new CookingRecordSummaryDto();
        dto.recordId      = e.getId();
        dto.recipeId       = e.getRecipe().getId();
        dto.recipeTitle    = e.getRecipe().getTitle();
        dto.ingredientCost = e.getRecipe().getTotalIngredientCost();
        dto.marketPrice    = e.getRecipe().getMarketPrice();
        dto.nutrition = NutritionSummaryDto.from(e);
        dto.calories = e.getTotalCalories();
        dto.imageUrl       = imageUrl;
        return dto;
    }
}