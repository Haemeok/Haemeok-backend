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
 * <p>V1 prod ({@code POST /api/recipes/{id}/private})는 PUBLIC↔PRIVATE 단순 토글이지만
 * dev V3는 <b>PUBLIC | PRIVATE 명시 지정</b> + 의미별 helper(applyPublicListed/applyPublicUnlisted/applyPrivate)로
 * 트리플(visibility/listingStatus/isPrivate) 동기화.
 *
 * <p>PUBLIC 전환은 origin 여부로 분기:
 * <ul>
 *   <li>일반 원본 (origin 없음) → applyPublicListed (검색/추천 노출)</li>
 *   <li>리믹스 (origin 있음) → applyPublicUnlisted (link-only — 검색/추천에 갑자기 노출되지 않음)</li>
 * </ul>
 *
 * <p><b>RESTRICTED는 deprecated</b>: ACL 기능 부재로 신규 사용 중지 — 외부 입력은 항상 INVALID_INPUT_VALUE(901)로 거부.
 * 기존 DB row 디시리얼라이즈 호환을 위해 enum 값 자체는 보존되지만 신규 토글은 불가능.
 */
@Tag(name = "Dev Recipe Visibility API",
        description = "레시피 가시성 변경 (dev V3) — PUBLIC | PRIVATE 명시 지정 + 트리플 동기화. RESTRICTED는 deprecated.")
@RestController
@RequestMapping("/api/dev/recipes")
@RequiredArgsConstructor
public class DevRecipeVisibilityController {

    private final DevRecipeVisibilityService devRecipeVisibilityService;

    @PatchMapping("/{recipeId}/visibility")
    @Operation(summary = "레시피 가시성 변경 (dev V3)",
            description = "허용 visibility: **PUBLIC | PRIVATE only**. RESTRICTED는 deprecated — 항상 INVALID_INPUT_VALUE(901)로 거부됨. " +
                    "PUBLIC 전환은 origin 여부로 자동 분기 (일반 원본은 검색/추천 노출, 리믹스는 link-only). " +
                    "응답에는 갱신된 visibility / isPrivate 반환.")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "변경 성공 — 갱신된 visibility / isPrivate 반환"),
            @ApiResponse(responseCode = "400",
                    description = "AI 생성 레시피를 이미지 없이 PUBLIC으로 변경 불가 " +
                            "(errorCode=208 CANNOT_MAKE_PUBLIC_WITHOUT_IMAGE) " +
                            "또는 visibility 값 누락/형식 오류 " +
                            "또는 RESTRICTED 입력 (errorCode=901 INVALID_INPUT_VALUE — RESTRICTED는 항상 거부됨)"),
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
