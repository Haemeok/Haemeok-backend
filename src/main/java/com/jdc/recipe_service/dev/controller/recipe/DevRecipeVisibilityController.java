package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevVisibilityUpdateRequest;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevVisibilityUpdateResponse;
import com.jdc.recipe_service.dev.service.recipe.DevRecipeVisibilityService;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

/**
 * Dev V3 레시피 가시성 변경 API.
 *
 * V1 prod (`POST /api/recipes/{id}/private`)는 PUBLIC↔PRIVATE 단순 토글이지만
 * dev V3는 **PUBLIC | PRIVATE | RESTRICTED 명시 지정** + applyVisibility 트리플 동기화.
 *
 * RESTRICTED는 feature flag {@code dev.visibility.restricted-enabled}로 환경별 통제:
 *  - dev 검증 환경(application-local.yml): true → RESTRICTED 허용
 *  - 운영 환경(application.yml default): false → RESTRICTED 요청 시 INVALID_INPUT_VALUE(901).
 *    운영 검색/목록이 isPrivate=false 기준이라 RESTRICTED가 노출될 위험을 방지.
 */
@Tag(name = "Dev Recipe Visibility API",
        description = "레시피 가시성 변경 (dev V3) — 명시적 visibility 지정 + 트리플(visibility/listingStatus/isPrivate) 동기화")
@RestController
@RequestMapping("/api/dev/recipes")
@RequiredArgsConstructor
public class DevRecipeVisibilityController {

    private final DevRecipeVisibilityService devRecipeVisibilityService;

    @PatchMapping("/{recipeId}/visibility")
    @Operation(summary = "레시피 가시성 변경 (dev V3)",
            description = "허용 visibility: **PUBLIC | PRIVATE | RESTRICTED**. " +
                    "트리플(visibility + listingStatus + isPrivate)이 함께 동기화되어 응답에 반환됨. " +
                    "RESTRICTED는 feature flag(`dev.visibility.restricted-enabled`)가 false인 환경에서는 " +
                    "INVALID_INPUT_VALUE(901)로 거부 — 운영 검색 노출 방지용.")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "변경 성공 — 갱신된 트리플 상태 반환"),
            @ApiResponse(responseCode = "400",
                    description = "AI 생성 레시피를 이미지 없이 PUBLIC으로 변경 불가 " +
                            "(errorCode=208 CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE) " +
                            "또는 visibility 값 누락/형식 오류 " +
                            "또는 RESTRICTED 요청이지만 dev.visibility.restricted-enabled=false " +
                            "(errorCode=901 INVALID_INPUT_VALUE)"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (errorCode=103 UNAUTHORIZED)"),
            @ApiResponse(responseCode = "403", description = "본인 레시피가 아님 (errorCode=202 RECIPE_ACCESS_DENIED)"),
            @ApiResponse(responseCode = "404", description = "레시피 없음 (errorCode=201 RECIPE_NOT_FOUND)")
    })
    public ResponseEntity<DevVisibilityUpdateResponse> updateVisibility(
            @Parameter(description = "레시피 ID (HashID 인코딩)") @DecodeId("recipeId") Long recipeId,
            @Valid @RequestBody DevVisibilityUpdateRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();
        DevVisibilityUpdateResponse response = devRecipeVisibilityService.updateVisibility(
                recipeId, request.visibility(), userId);

        return ResponseEntity.ok(response);
    }
}
