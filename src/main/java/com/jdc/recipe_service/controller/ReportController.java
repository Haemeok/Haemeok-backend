package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.report.IngredientReportRequest;
import com.jdc.recipe_service.domain.dto.report.ReportResponse;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.ReportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/recipes")
@Tag(name = "오류 신고 API", description = "레시피 재료 오류 신고를 위한 API입니다.")
public class ReportController {

    private final ReportService reportService;

    @PostMapping("/{recipeId}/ingredients/{ingredientId}/reports")
    @Operation(summary = "재료 오류 신고", description = "특정 레시피의 재료에 대한 오류(양, 이름, 없음 등)를 신고합니다.")
    public ResponseEntity<String> reportIngredientError(
            @Parameter(description = "레시피 ID") @DecodeId Long recipeId,
            @Parameter(description = "재료 ID") @DecodeId Long ingredientId,
            @RequestBody @Valid IngredientReportRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        reportService.createReport(recipeId, ingredientId, userDetails.getUser().getId(), request);
        return ResponseEntity.ok("소중한 제보 감사합니다.");
    }

    @GetMapping("/reports")
    @Operation(summary = "오류 신고 목록 조회 (관리자 전용)", description = "접수된 모든 재료 오류 신고 내역을 조회합니다.")
    public ResponseEntity<List<ReportResponse>> getAllReports(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestParam(required = false, defaultValue = "false") boolean onlyUnresolved
    ) {
        if (userDetails == null || userDetails.getUser().getRole() != Role.ADMIN) {
            throw new CustomException(ErrorCode.ADMIN_ACCESS_DENIED);
        }

        return ResponseEntity.ok(reportService.getAllReports(onlyUnresolved));
    }
}