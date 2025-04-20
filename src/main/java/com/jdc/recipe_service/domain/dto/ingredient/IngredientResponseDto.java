package com.jdc.recipe_service.domain.dto.ingredient;

import lombok.*;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor
@Builder
public class IngredientResponseDto {
    private Long id;
    private String name;
    private String category;
    private String imageUrl;
    private Integer price;
    private String unit;
}