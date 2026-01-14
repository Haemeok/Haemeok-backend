package com.jdc.recipe_service.domain.dto.calendar;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
public class CookingRecordDto {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;
    @JsonSerialize(using = HashIdSerializer.class)
    private Long recipeId;
    private String recipeTitle;
    private Integer ingredientCost;
    private Integer marketPrice;
    private Integer savings;
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
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