package com.jdc.recipe_service.dev.controller.report;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.report.DevReportService;
import com.jdc.recipe_service.domain.dto.report.IngredientReportRequest;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Dev V3 재료 신고 API.
 *
 * 운영 {@code POST /api/recipes/{recipeId}/reports} 미러. dev V3 차이점:
 *  - **🚨 운영 leak 차단**: 운영은 recipe visibility 검사 없이 report 생성 → dev V3는 가시성 게이트 적용
 *
 * 관리자 GET endpoint는 dev mirror 불필요 (admin은 운영 사용).
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/recipes")
@Tag(name = "Dev V3 재료 신고 API",
        description = "create는 recipe 가시성 게이트 (RESTRICTED non-owner / non-ACTIVE 차단)")
public class DevReportController {

    private final DevReportService devReportService;

    @PostMapping("/{recipeId}/reports")
    @Operation(summary = "Dev V3 재료 제보 (이름 기반)",
            description = """
                    운영 `POST /api/recipes/{recipeId}/reports` 미러. dev V3 차이점:
                      - **🚨 운영 leak 차단**: 운영 ReportService.createReportByName는 recipe visibility 검사 없이 report 생성 →
                        RESTRICTED/PRIVATE non-owner / non-ACTIVE 레시피에도 report 가능. dev V3가 가시성 게이트로 차단
                      - 게이트 통과 시 운영 service에 위임 (재료 매칭 + 신고 row 생성)
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "신고 성공"),
            @ApiResponse(responseCode = "400", description = "payload 검증 실패 (INVALID_INPUT_VALUE)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "PRIVATE/RESTRICTED 레시피 non-owner (RECIPE_PRIVATE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<String> reportIngredient(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @Valid @RequestBody IngredientReportRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        devReportService.createReportByName(userId, recipeId, request);
        return ResponseEntity.ok("소중한 제보 감사합니다.");
    }
}
