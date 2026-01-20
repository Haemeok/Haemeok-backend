package com.jdc.recipe_service.domain.dto.report;

import com.jdc.recipe_service.domain.type.ReportReason;
import jakarta.validation.constraints.NotNull;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class IngredientReportRequest {

    @NotNull(message = "신고 사유는 필수입니다.")
    private ReportReason reason;
    private String memo;
}