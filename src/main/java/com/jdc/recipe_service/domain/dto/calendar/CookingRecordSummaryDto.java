package com.jdc.recipe_service.domain.dto.calendar;

import lombok.Getter;

@Getter
public class CookingRecordSummaryDto {
    private Long recordId;
    private Long recipeId;
    private String recipeTitle;
    private Integer ingredientCost;
    private Integer marketPrice;
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
        dto.imageUrl       = imageUrl;
        return dto;
    }
}