package com.jdc.recipe_service.domain.dto.calendar;

import lombok.Getter;

@Getter
public class CookingRecordDto {
    private Long id;
    private Long recipeId;
    private String recipeTitle;
    private Integer ingredientCost;
    private Integer marketPrice;
    private Integer savings;
    private String createdAt;

    public static CookingRecordDto from(com.jdc.recipe_service.domain.entity.CookingRecord e) {
        var dto = new CookingRecordDto();
        dto.id             = e.getId();
        dto.recipeId       = e.getRecipe().getId();
        dto.recipeTitle    = e.getRecipe().getTitle();
        dto.ingredientCost = e.getIngredientCost();
        dto.marketPrice    = e.getMarketPrice();
        dto.savings        = e.getSavings();
        dto.createdAt      = e.getCreatedAt().toString();
        return dto;
    }
}