package com.jdc.recipe_service.dev.controller.interaction;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.interaction.DevInteractionService;
import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
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
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

/**
 * Dev V3 like/favorite toggle API.
 *
 * 운영 {@code /api/recipes/*}의 like/favorite와 동일한 응답 shape (Map<liked|saved, message>).
 * 차이점은 {@link DevInteractionService}의 분기 게이트:
 *  - **신규 추가 시에만** {@link DevRecipeAccessValidator}로 PRIVATE/RESTRICTED non-owner / non-ACTIVE 차단
 *  - **기존 interaction 제거 경로**는 게이트 우회 — 본인 데이터 정리 권한 보장
 *  (운영은 visibility 검증 없이 누구나 like/favorite 가능 — 잠재 누수를 신규 경로에서만 닫는다)
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/recipes")
@Tag(name = "Dev V3 좋아요/즐겨찾기 API",
        description = "운영보다 엄격한 신규 권한 게이트 — RESTRICTED/PRIVATE non-owner 신규 상호작용 차단, 기존은 정리 가능")
public class DevLikeFavoriteController {

    private final DevInteractionService devInteractionService;

    @PostMapping("/{id}/like")
    @Operation(summary = "Dev V3 레시피 좋아요 토글",
            description = """
                    운영 `POST /api/recipes/{id}/like` 미러. dev V3 차이점:
                      - **신규 추가 시 권한 게이트**: 기존 like 없을 때만 PRIVATE/RESTRICTED non-owner → 403, non-ACTIVE → 404
                      - **기존 like 제거 경로는 항상 허용**: 레시피가 나중에 RESTRICTED/PRIVATE/non-ACTIVE로 바뀌어도 본인 like 정리 가능
                      - 통과 시 운영 RecipeLikeService에 위임 (알림 생성 포함)
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "토글 성공 — {liked: boolean, message: string}"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "신규 추가 시 PRIVATE/RESTRICTED 레시피 non-owner 시도 (RECIPE_PRIVATE_ACCESS_DENIED)",
                    content = @Content),
            @ApiResponse(responseCode = "404", description = "신규 추가 시 레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Map<String, Object>> toggleLike(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        boolean liked = devInteractionService.toggleLike(userId, id);
        return ResponseEntity.ok(Map.of(
                "liked", liked,
                "message", liked ? "레시피 좋아요 등록 완료" : "레시피 좋아요 취소 완료"
        ));
    }

    @PostMapping("/{id}/favorite")
    @Operation(summary = "Dev V3 레시피 저장 토글",
            description = """
                    운영 `POST /api/recipes/{id}/favorite` 미러. dev V3 차이점:
                      - **신규 추가 시 권한 게이트**: 기존 저장 없을 때만 PRIVATE/RESTRICTED non-owner → 403, non-ACTIVE → 404
                      - **기존 저장 제거 경로는 항상 허용**: 레시피가 나중에 RESTRICTED/PRIVATE/non-ACTIVE로 바뀌어도 폴더에서 정리 가능
                      - 통과 시 운영 RecipeBookService.toggleSave에 위임 (기본 폴더 추가/제거 + legacy favorite 동기화)
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "토글 성공 — {saved: boolean, message: string}"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "신규 추가 시 PRIVATE/RESTRICTED 레시피 non-owner 시도 (RECIPE_PRIVATE_ACCESS_DENIED)",
                    content = @Content),
            @ApiResponse(responseCode = "404", description = "신규 추가 시 레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Map<String, Object>> toggleFavorite(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        boolean saved = devInteractionService.toggleFavorite(userId, id);
        return ResponseEntity.ok(Map.of(
                "saved", saved,
                "message", saved ? "레시피 저장 완료" : "레시피 저장 해제 완료"
        ));
    }
}
