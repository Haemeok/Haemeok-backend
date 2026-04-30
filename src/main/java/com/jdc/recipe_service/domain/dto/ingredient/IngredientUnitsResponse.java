package com.jdc.recipe_service.domain.dto.ingredient;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "재료별 선택 가능한 단위 목록")
public class IngredientUnitsResponse {

    @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
    @Schema(description = "재료 ID (HashID)", example = "xJvY7aBp")
    private Long ingredientId;

    @Schema(description = "해당 재료에서 선택 가능한 단위 목록")
    private List<IngredientUnitDto> units;
}
