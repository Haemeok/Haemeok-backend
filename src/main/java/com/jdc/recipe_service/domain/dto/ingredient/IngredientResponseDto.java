package com.jdc.recipe_service.domain.dto.ingredient;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.*;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor
@Builder
public class IngredientResponseDto {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;
    private String name;
    private String category;
    private String imageUrl;
    private Integer price;
    private String unit;
    private boolean inFridge;
}