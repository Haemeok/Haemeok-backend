package com.jdc.recipe_service.domain.dto.ingredient;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Ingredient nutrition values per 100g. Null fields mean the master data is not available yet.")
public class IngredientNutritionPer100gDto {

    @Schema(description = "Calories per 100g (kcal)", example = "77.000", nullable = true)
    private BigDecimal kcal;

    @Schema(description = "Carbohydrate per 100g (g)", example = "17.500", nullable = true)
    private BigDecimal carbohydrateG;

    @Schema(description = "Protein per 100g (g)", example = "2.000", nullable = true)
    private BigDecimal proteinG;

    @Schema(description = "Fat per 100g (g)", example = "0.100", nullable = true)
    private BigDecimal fatG;

    @Schema(description = "Sugar per 100g (g)", example = "0.800", nullable = true)
    private BigDecimal sugarG;

    @Schema(description = "Sodium per 100g (mg)", example = "6.000", nullable = true)
    private BigDecimal sodiumMg;
}
