package com.jdc.recipe_service.domain.dto.calendar;

import lombok.Getter;

@Getter
public class CookingRecordSummaryDto {
    private Long recipeId;
    private String recipeTitle;
    private Integer savings;
    private String imageUrl;

    public static CookingRecordSummaryDto from(
            com.jdc.recipe_service.domain.entity.CookingRecord e,
                                               String imageUrl
    ) {
        var dto = new CookingRecordSummaryDto();
        dto.recipeId       = e.getRecipe().getId();
        dto.recipeTitle    = e.getRecipe().getTitle();
        dto.savings        = e.getSavings();
        dto.imageUrl       = imageUrl;
        return dto;
    }
}