package com.jdc.recipe_service.domain.dto.report;

import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Getter
@NoArgsConstructor
public class AdminIngredientUpdateDto {
    private Long id;
    private String name;
    private String quantity;
    private String unit;
    private Integer price;
    private String action;

    private BigDecimal calorie;
    private BigDecimal carbohydrate;
    private BigDecimal protein;
    private BigDecimal fat;
    private BigDecimal sugar;
    private BigDecimal sodium;
}