package com.jdc.recipe_service.domain.dto.recipe.ingredient;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.math.BigDecimal;


/**
 *
 *  재료 요청용
 */

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeIngredientRequestDto {
    private String name;
    private String quantity;

    private BigDecimal customPrice;
    @JsonProperty("unit")
    private String customUnit;

    private BigDecimal customCalories;

    private BigDecimal customCarbohydrate;
    private BigDecimal customProtein;
    private BigDecimal customFat;
    private BigDecimal customSugar;
    private BigDecimal customSodium;
}