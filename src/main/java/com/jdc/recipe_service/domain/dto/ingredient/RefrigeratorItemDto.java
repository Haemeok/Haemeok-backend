package com.jdc.recipe_service.domain.dto.ingredient;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RefrigeratorItemDto {
    private Long ingredientId;
    private String name;
}