package com.jdc.recipe_service.domain.dto.report;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Getter
@NoArgsConstructor
public class AdminIngredientUpdateDto {
    @JsonDeserialize(using = HashIdConfig.HashIdDeserializer.class)
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