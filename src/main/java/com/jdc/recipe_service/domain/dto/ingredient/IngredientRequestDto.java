package com.jdc.recipe_service.domain.dto.ingredient;

import jakarta.validation.constraints.NotBlank;
import lombok.*;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor
@Builder
public class IngredientRequestDto {
    @NotBlank
    private String name;
    private String category;
    private String imageUrl;
    private Integer price;
    private String unit;
}