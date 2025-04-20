package com.jdc.recipe_service.domain.dto.fridge;

import lombok.Data;
import jakarta.validation.constraints.NotEmpty;
import java.util.List;

@Data
public class RefrigeratorItemBulkRequestDto {
    @NotEmpty
    private List<Long> ingredientIds;
}