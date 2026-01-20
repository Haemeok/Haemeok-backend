package com.jdc.recipe_service.domain.dto.report;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import com.jdc.recipe_service.domain.entity.RecipeIngredientReport;
import com.jdc.recipe_service.domain.type.ReportReason;
import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;

@Getter
@Builder
public class ReportResponse {

    private Long reportId;

    @JsonSerialize(using = HashIdSerializer.class)
    private Long recipeId;

    @JsonSerialize(using = HashIdSerializer.class)
    private Long ingredientId;

    @JsonSerialize(using = HashIdSerializer.class)
    private Long memberId;

    private ReportReason reason;
    private String memo;
    private boolean isResolved;
    private LocalDateTime createdAt;

    public static ReportResponse from(RecipeIngredientReport entity) {
        return ReportResponse.builder()
                .reportId(entity.getId())
                .recipeId(entity.getRecipeId())
                .ingredientId(entity.getIngredientId())
                .memberId(entity.getMemberId())
                .reason(entity.getReason())
                .memo(entity.getUserMemo())
                .isResolved(entity.isResolved())
                .createdAt(entity.getCreatedAt())
                .build();
    }
}