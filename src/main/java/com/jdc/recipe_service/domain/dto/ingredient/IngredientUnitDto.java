package com.jdc.recipe_service.domain.dto.ingredient;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "재료별 선택 가능한 단위")
public class IngredientUnitDto {

    @Schema(description = "화면 표시 및 레시피 저장 시 ingredients[].unit으로 다시 전달할 단위명", example = "개")
    private String unit;

    @Schema(description = "사용자 표시용 총중량 기준 g", example = "150.000")
    private BigDecimal gramsPerUnit;

    @Schema(description = "해당 재료의 기본 선택 단위 여부", example = "true")
    private Boolean isDefault;
}
