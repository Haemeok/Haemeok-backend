package com.jdc.recipe_service.domain.dto.recipe.ingredient;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;


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
    private LocalDateTime coupangLinkUpdatedAt;
}
