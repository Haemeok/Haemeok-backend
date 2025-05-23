package com.jdc.recipe_service.domain.dto.calendar;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
public class CookingRecordDto {
    private Long id;
    private Long recipeId;
    private String recipeTitle;
    private Integer ingredientCost;
    private Integer marketPrice;
    private Integer savings;
    @JsonFormat(
            shape = JsonFormat.Shape.STRING,
            pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'",
            timezone = "UTC"
    )
    private LocalDateTime createdAt;

    public static CookingRecordDto from(com.jdc.recipe_service.domain.entity.CookingRecord e) {
        var dto = new CookingRecordDto();
        dto.id             = e.getId();
        dto.recipeId       = e.getRecipe().getId();
        dto.recipeTitle    = e.getRecipe().getTitle();
        dto.ingredientCost = e.getIngredientCost();
        dto.marketPrice    = e.getMarketPrice();
        dto.savings        = e.getSavings();
        dto.createdAt      = e.getCreatedAt();
        return dto;
    }
}