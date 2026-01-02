package com.jdc.recipe_service.domain.dto.fridge;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import lombok.Data;
import jakarta.validation.constraints.NotEmpty;
import java.util.List;

@Data
public class RefrigeratorItemBulkRequestDto {
    @NotEmpty
    @JsonDeserialize(contentUsing = HashIdConfig.HashIdDeserializer.class)
    private List<Long> ingredientIds;
}