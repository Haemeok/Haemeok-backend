package com.jdc.recipe_service.domain.dto.recipe.ingredient;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.*;

import java.math.BigDecimal;


@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeIngredientDto {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;
    private String name;
    private String quantity;
    private String unit;
    private Integer price;
    private Double calories;
    private String coupangLink;

    // 커스텀(YouTube/AI 생성) 재료 per-unit 원본값. 수정 시 round-trip 보존용.
    private BigDecimal customPrice;
    private BigDecimal customCalories;
    private BigDecimal customCarbohydrate;
    private BigDecimal customProtein;
    private BigDecimal customFat;
    private BigDecimal customSugar;
    private BigDecimal customSodium;
}
