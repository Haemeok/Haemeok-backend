package com.jdc.recipe_service.domain.dto.fridge;

import jakarta.validation.constraints.NotNull;
import lombok.*;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
public class RefrigeratorItemRequestDto {
    @NotNull
    private Long ingredientId;
}