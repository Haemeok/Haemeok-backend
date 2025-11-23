package com.jdc.recipe_service.domain.dto.calendar;

import lombok.Getter;
import lombok.Setter;
import java.math.BigDecimal;

@Getter @Setter
public class NutritionSummaryDto {
    private BigDecimal protein;
    private BigDecimal carbohydrate;
    private BigDecimal fat;
    private BigDecimal sugar;
    private BigDecimal sodium;

    public static NutritionSummaryDto from(com.jdc.recipe_service.domain.entity.CookingRecord e) {
        NutritionSummaryDto dto = new NutritionSummaryDto();
        dto.protein = e.getProtein();
        dto.carbohydrate = e.getCarbohydrate();
        dto.fat = e.getFat();
        dto.sugar = e.getSugar();
        dto.sodium = e.getSodium();
        return dto;
    }
}