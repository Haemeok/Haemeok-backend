package com.jdc.recipe_service.domain.dto.ingredient;

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
@Schema(description = "여러 재료의 선택 가능한 단위 목록 조회 응답")
public class IngredientUnitsBatchResponse {

    @Schema(description = "입력 재료별 단위 목록")
    private List<IngredientUnitsResponse> items;
}
