package com.jdc.recipe_service.domain.dto.ingredient;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "여러 재료의 선택 가능한 단위 목록 조회 요청")
public class IngredientUnitsBatchRequest {

    @NotEmpty
    @JsonDeserialize(contentUsing = HashIdConfig.HashIdDeserializer.class)
    @Schema(description = "재료 ID(HashID) 목록", example = "[\"xJvY7aBp\", \"vK9mP2Qa\"]")
    private List<Long> ingredientIds;
}
