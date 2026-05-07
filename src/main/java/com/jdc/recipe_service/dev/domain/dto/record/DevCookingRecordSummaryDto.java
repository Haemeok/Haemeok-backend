package com.jdc.recipe_service.dev.domain.dto.record;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import com.jdc.recipe_service.domain.dto.calendar.CookingRecordSummaryDto;
import com.jdc.recipe_service.domain.dto.calendar.NutritionSummaryDto;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Builder;
import lombok.Getter;

import java.math.BigDecimal;

/** Dev V3 요리 기록 요약 DTO. timeline + calendar?date= 응답에서 사용. */
@Getter
@Builder
@Schema(description = "Dev V3 요리 기록 요약")
public class DevCookingRecordSummaryDto {

    @JsonSerialize(using = HashIdSerializer.class)
    @Schema(description = "기록 ID")
    private Long recordId;

    @JsonSerialize(using = HashIdSerializer.class)
    @Schema(description = "레시피 ID")
    private Long recipeId;

    @Schema(description = "레시피 제목")
    private String recipeTitle;

    @Schema(description = "재료 비용")
    private Integer ingredientCost;

    @Schema(description = "시장가")
    private Integer marketPrice;

    @Schema(description = "영양 요약")
    private NutritionSummaryDto nutrition;

    @Schema(description = "총 칼로리")
    private BigDecimal calories;

    @Schema(description = "이미지 URL")
    private String imageUrl;

    @Schema(description = "레시피 가시성 (현재 시점 기준)", example = "PUBLIC")
    private String visibility;

    // 필드명 'remix' — Lombok isRemix() getter가 Jackson에 "remix"로 이중 등록되는 것 회피.
    @Schema(description = "remix(클론) 레시피 여부", example = "true")
    @JsonProperty("isRemix")
    private boolean remix;

    public static DevCookingRecordSummaryDto fromRaw(CookingRecordSummaryDto raw,
                                                      String visibility,
                                                      boolean isRemix) {
        return DevCookingRecordSummaryDto.builder()
                .recordId(raw.getRecordId())
                .recipeId(raw.getRecipeId())
                .recipeTitle(raw.getRecipeTitle())
                .ingredientCost(raw.getIngredientCost())
                .marketPrice(raw.getMarketPrice())
                .nutrition(raw.getNutrition())
                .calories(raw.getCalories())
                .imageUrl(raw.getImageUrl())
                .visibility(visibility)
                .remix(isRemix)
                .build();
    }
}
