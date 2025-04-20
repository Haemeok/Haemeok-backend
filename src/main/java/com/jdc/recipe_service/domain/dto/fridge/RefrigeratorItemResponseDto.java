package com.jdc.recipe_service.domain.dto.fridge;

import com.jdc.recipe_service.domain.dto.ingredient.IngredientResponseDto;
import lombok.*;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
public class RefrigeratorItemResponseDto {
    private Long id;
    private IngredientResponseDto ingredient;
    private String createdAt;
    private String updatedAt;
}