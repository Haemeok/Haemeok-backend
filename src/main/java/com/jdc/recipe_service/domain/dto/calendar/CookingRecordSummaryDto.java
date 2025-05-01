package com.jdc.recipe_service.domain.dto.calendar;

import lombok.Getter;

@Getter
public class CookingRecordSummaryDto {
    private Long recipeId;
    private String recipeTitle;
    private Integer ingredientCost;
    private Integer marketPrice;
    private Integer savings;

    public static CookingRecordSummaryDto from(com.jdc.recipe_service.domain.entity.CookingRecord e) {
        var dto = new CookingRecordSummaryDto();
        dto.recipeId       = e.getRecipe().getId();
        dto.recipeTitle    = e.getRecipe().getTitle();
        dto.ingredientCost = e.getIngredientCost();
        dto.marketPrice    = e.getMarketPrice();
        dto.savings        = e.getSavings();
        return dto;
    }
}