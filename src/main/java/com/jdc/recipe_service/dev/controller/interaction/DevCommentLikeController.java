package com.jdc.recipe_service.dev.controller.interaction;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.interaction.DevCommentLikeService;
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
 * Dev V3 댓글 좋아요 API.
 *
 * 운영 {@code POST /api/comments/{commentId}/like} 미러. dev V3 차이점은 {@link DevCommentLikeService}가 담당:
 *  - 신규 like: comment → recipe 추적 → 가시성 게이트 (RESTRICTED non-owner / non-ACTIVE 차단)
 *  - 기존 like 제거: 게이트 없음 (cleanup right)
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/comments")
@Tag(name = "Dev V3 댓글 좋아요 API",
        description = "댓글 좋아요 신규 등록 시 댓글이 달린 레시피 가시성 게이트 — RESTRICTED 댓글 like 누설 차단")
public class DevCommentLikeController {

    private final DevCommentLikeService devCommentLikeService;

    @PostMapping("/{commentId}/like")
    @Operation(summary = "Dev V3 댓글 좋아요 토글",
            description = """
                    운영 `POST /api/comments/{commentId}/like` 미러. dev V3 차이점:
                      - **신규 like 시 게이트**: 댓글이 달린 레시피가 PRIVATE/RESTRICTED non-owner → 403, non-ACTIVE → 404
                      - **기존 like 제거**: 게이트 없음 (cleanup right — 본인 like는 항상 정리 가능)
                      - 통과 시 운영 CommentLikeService에 위임
                      - **인증 필수**

                    **잔여 정보 노출 (수용된 trade-off)**: 사용자가 예전에 좋아요했던 댓글의 레시피가 나중에
                    RESTRICTED/PRIVATE/non-ACTIVE가 된 경우, unlike(cleanup) 응답에 현재 `likeCount`가 그대로 포함된다.
                    likeCount는 익명 합산값이라 누가 like했는지는 노출하지 않으며, 운영 응답 shape parity를 우선해서
                    수용한다. 더 엄격하게 닫으려면 service에서 cleanup-inaccessible 케이스를 분기해 likeCount를 숨겨야 한다.
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "토글 성공 — {liked: boolean, likeCount: int, message: string}"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "신규 like 시 PRIVATE/RESTRICTED non-owner (RECIPE_PRIVATE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "댓글 없음 (COMMENT_NOT_FOUND) 또는 신규 like 시 레시피 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Map<String, Object>> toggleLike(
            @Parameter(description = "댓글 ID (HashID)") @DecodeId("commentId") Long commentId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        boolean liked = devCommentLikeService.toggleLike(userId, commentId);
        int likeCount = devCommentLikeService.countLikes(commentId);
        return ResponseEntity.ok(Map.of(
                "liked", liked,
                "likeCount", likeCount,
                "message", liked ? "댓글 좋아요 등록 완료" : "댓글 좋아요 취소 완료"
        ));
    }
}
