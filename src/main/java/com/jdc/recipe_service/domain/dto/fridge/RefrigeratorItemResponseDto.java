package com.jdc.recipe_service.domain.dto.fridge;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientResponseDto;
import lombok.*;

import java.time.LocalDateTime;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
public class RefrigeratorItemResponseDto {
    private Long id;
    private IngredientResponseDto ingredient;
    @JsonFormat(
            shape = JsonFormat.Shape.STRING,
            pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'",
            timezone = "UTC"
    )
    private LocalDateTime createdAt;
}