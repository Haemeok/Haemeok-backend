package com.jdc.recipe_service.domain.dto.fridge;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import jakarta.validation.constraints.NotNull;
import lombok.*;

@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
public class RefrigeratorItemRequestDto {
    @NotNull
    @JsonDeserialize(using = HashIdConfig.HashIdDeserializer.class)
    private Long ingredientId;
}