package com.jdc.recipe_service.domain.dto.recipe.ingredient;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;

import java.math.BigDecimal;


/**
 *
 *  재료 요청용
 */

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeIngredientRequestDto {
    private String name;
    private String quantity;

    private BigDecimal customPrice;
    @JsonProperty("unit")
    private String customUnit;

    private BigDecimal customCalories;

    private BigDecimal customCarbohydrate;
    private BigDecimal customProtein;
    private BigDecimal customFat;
    private BigDecimal customSugar;
    private BigDecimal customSodium;
    private Boolean isEstimated;

    /**
     * dev V3 전용 — 사용자가 의도적으로 마스터 매핑을 거부했음을 표시.
     *
     * <p>true: 마스터 lookup 시도 자체를 건너뛰고 CUSTOM으로 저장 (예: "엄마표 양념", 브랜드 명시).
     * <p>false/null: 시스템 매칭 시도. 실패하면 UNRESOLVED, 부분 성공하면 PARTIAL, 완전 성공하면 MAPPED.
     *
     * <p>operational(V1/V2)는 이 필드를 무시 — 추가 필드라 backward-compatible.
     * AI/YouTube path는 항상 false 강제 (시스템 실패는 UNRESOLVED이지 사용자 의도 CUSTOM 아님).
     */
    @Schema(description = """
            (dev V3) 사용자가 마스터 매핑을 의도적으로 거부했는지 여부.
            true면 시스템 매칭을 건너뛰고 CUSTOM 라인으로 저장 (예: "엄마표 양념", 브랜드 명시 재료).
            false/null이면 시스템 매칭 시도 — 결과는 MAPPED / PARTIAL / UNRESOLVED 중 하나.
            AI/YouTube 추출 path는 항상 false로 강제됨 (시스템 매칭 실패는 UNRESOLVED이지 사용자 의도 CUSTOM이 아님).
            operational(V1/V2)는 이 필드를 무시한다 — backward-compatible.""",
            example = "false",
            nullable = true)
    private Boolean customByUser;
}