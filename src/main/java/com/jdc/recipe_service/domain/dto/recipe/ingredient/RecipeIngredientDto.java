package com.jdc.recipe_service.domain.dto.recipe.ingredient;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeIngredientDto {
    private Long id;
    private String name;
    private String quantity;
    private String unit;
    private Integer price;
    private Double calories;
    private String coupangLink;
}
