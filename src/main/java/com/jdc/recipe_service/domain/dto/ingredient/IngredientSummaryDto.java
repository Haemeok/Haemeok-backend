package com.jdc.recipe_service.domain.dto.ingredient;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class IngredientSummaryDto {
        @JsonSerialize(using = HashIdSerializer.class)
        private Long id;
        private String name;
        private String category;
        private String imageUrl;
        private String unit;
        private Boolean inFridge;
}