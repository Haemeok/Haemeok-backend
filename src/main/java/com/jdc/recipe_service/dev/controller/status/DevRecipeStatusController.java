package com.jdc.recipe_service.dev.controller.status;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.status.DevRecipeStatusService;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeSaveStatusResponse;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeDetailStatusDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStatusDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeStatusRequest;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;
import java.util.stream.Collectors;

/**
 * Dev V3 status/check API.
 *
 * 운영 {@code /api/v2/recipes/{id}/status}, {@code POST /api/v2/recipes/status},
 * {@code /api/recipes/{id}/saved-books} 미러. dev V3 차이점:
 *  - 단건 status: 가시성 게이트 — RESTRICTED non-owner / non-ACTIVE 차단
 *  - 배치 status: 접근 불가 ID는 응답에서 silently 제외 (per-id throw 안 함)
 *  - saved-books: 본인 collection 메타라 게이트 없음 (단순 위임)
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/recipes")
@Tag(name = "Dev V3 status/check API",
        description = "단건/배치 동적 상태 조회 + 본인 저장 폴더 조회. 가시성 누수 차단.")
public class DevRecipeStatusController {

    private final DevRecipeStatusService devRecipeStatusService;
    private final Hashids hashids;

    @GetMapping("/{id}/status")
    @Operation(summary = "Dev V3 레시피 상세 상태 조회",
            description = """
                    운영 `GET /api/v2/recipes/{id}/status` 미러. dev V3 차이점:
                      - **권한 게이트**: PRIVATE/RESTRICTED non-owner → 403, non-ACTIVE → 404
                      - 통과 시 운영 RecipeStatusService에 위임 (좋아요/즐겨찾기/평점/댓글/냉장고/remix 본인 상태)
                      - 인증 선택 — 비로그인은 PUBLIC+LISTED+ACTIVE만 통과 (게이트 적용)
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공"),
            @ApiResponse(responseCode = "403", description = "PRIVATE/RESTRICTED 레시피 non-owner (RECIPE_PRIVATE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<RecipeDetailStatusDto> getDetailStatus(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("id") Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long viewerId = userDetails != null ? userDetails.getUser().getId() : null;
        return ResponseEntity.ok(devRecipeStatusService.getDetailStatus(viewerId, id));
    }

    @PostMapping("/status")
    @Operation(summary = "Dev V3 레시피 상태 배치 조회",
            description = """
                    운영 `POST /api/v2/recipes/status` 미러. dev V3 차이점:
                      - **silent filter**: 입력 ID 중 접근 불가능한 것은 결과에서 silently 제외 (per-id throw 안 함)
                      - 응답 Map에 없는 ID = 비공개/non-ACTIVE/존재 안 함 — 클라이언트 구분 불가 (의도적 정보 누설 차단)
                      - 인증 선택 — 비로그인은 PUBLIC+LISTED+ACTIVE만 결과에 포함
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 — Map<encodedHashId, {likedByCurrentUser, favoriteByCurrentUser}>")
    })
    public ResponseEntity<Map<String, RecipeSimpleStatusDto>> getBatchStatuses(
            @RequestBody RecipeStatusRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long viewerId = userDetails != null ? userDetails.getUser().getId() : null;

        Map<Long, RecipeSimpleStatusDto> raw =
                devRecipeStatusService.getBatchSimpleStatuses(viewerId, request.getRecipeIds());

        Map<String, RecipeSimpleStatusDto> encoded = raw.entrySet().stream()
                .collect(Collectors.toMap(
                        e -> hashids.encode(e.getKey()),
                        Map.Entry::getValue
                ));
        return ResponseEntity.ok(encoded);
    }

    @GetMapping("/{id}/saved-books")
    @Operation(summary = "Dev V3 레시피 저장 상태 조회",
            description = """
                    운영 `GET /api/recipes/{id}/saved-books` 미러. dev V3 차이점 없음 —
                    본인 collection 메타라 게이트 없음. 응답은 본인 폴더 정보만 (새 recipe 정보 누설 없음).
                    **인증 필수**.
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content)
    })
    public ResponseEntity<RecipeSaveStatusResponse> getSavedBooks(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("id") Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        return ResponseEntity.ok(devRecipeStatusService.getSaveStatus(userId, id));
    }
}
