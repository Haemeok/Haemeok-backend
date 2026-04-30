package com.jdc.recipe_service.dev.controller.interaction;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.interaction.DevCommentService;
import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.domain.dto.comment.CommentWithRepliesDto;
import com.jdc.recipe_service.domain.dto.comment.ReplyDto;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Dev V3 댓글 API.
 *
 * 운영 {@code /api/recipes/{recipeId}/comments/*} 미러. dev V3 차이점은 {@link DevCommentService}가 담당:
 *  - **POST create/reply**: 게이트 통과 후에만 운영 위임 → RESTRICTED non-owner 시도 시 알림 발생 자체 불가
 *  - **GET list/replies**: 게이트 적용 — RESTRICTED 레시피 댓글 직접 URL 접근 차단
 *  - **DELETE**: 게이트 없음 — 본인 comment 정리는 recipe visibility 무관 (운영 service가 ownership 체크)
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/recipes/{recipeId}/comments")
@Tag(name = "Dev V3 댓글 API",
        description = "댓글 생성/조회 시 권한 게이트 — RESTRICTED/PRIVATE 레시피 차단. 본인 댓글 삭제는 항상 허용.")
public class DevCommentController {

    private final DevCommentService devCommentService;

    @PostMapping
    @Operation(summary = "Dev V3 댓글 작성",
            description = """
                    운영 `POST /api/recipes/{recipeId}/comments` 미러. dev V3 차이점:
                      - **권한 게이트**: PRIVATE/RESTRICTED non-owner → 403, non-ACTIVE → 404. 게이트 통과 시에만 운영 service 위임 (알림 포함)
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "201", description = "작성 성공"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "PRIVATE/RESTRICTED 레시피 non-owner (RECIPE_PRIVATE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<CommentDto> createComment(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @Valid @RequestBody CommentRequestDto requestDto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        CommentDto created = devCommentService.createComment(userId, recipeId, requestDto, userDetails.getUser());
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @PostMapping("/{parentId}/replies")
    @Operation(summary = "Dev V3 대댓글 작성",
            description = """
                    운영 `POST /api/recipes/{recipeId}/comments/{parentId}/replies` 미러. dev V3 차이점:
                      - **권한 게이트**: PRIVATE/RESTRICTED non-owner → 403, non-ACTIVE → 404
                      - 게이트 통과 시에만 운영 service 위임 (대댓글 알림 포함). parentId가 다른 recipe 소속이면 COMMENT_NOT_FOUND
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "201", description = "작성 성공"),
            @ApiResponse(responseCode = "401", description = "인증 필요", content = @Content),
            @ApiResponse(responseCode = "403", description = "PRIVATE/RESTRICTED non-owner", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피/부모 댓글 없음 또는 non-ACTIVE", content = @Content)
    })
    public ResponseEntity<ReplyDto> createReply(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @Parameter(description = "부모 댓글 ID (HashID)") @DecodeId("parentId") Long parentId,
            @Valid @RequestBody CommentRequestDto requestDto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        ReplyDto created = devCommentService.createReply(userId, recipeId, parentId, requestDto, userDetails.getUser());
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @GetMapping
    @Operation(summary = "Dev V3 댓글 목록",
            description = """
                    운영 `GET /api/recipes/{recipeId}/comments` 미러. dev V3 차이점:
                      - **권한 게이트** (운영보다 엄격): anonymous는 PUBLIC+LISTED+ACTIVE 레시피만 댓글 조회 가능. RESTRICTED/PRIVATE/non-ACTIVE 직접 URL 접근 차단.
                      - 정렬: `createdAt` (default DESC) | `likeCount`
                      - 인증 선택 — 로그인 시 본인 like 여부 표시
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공"),
            @ApiResponse(responseCode = "403", description = "PRIVATE/RESTRICTED non-owner", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 또는 non-ACTIVE", content = @Content)
    })
    public ResponseEntity<Page<CommentDto>> getAllComments(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @PageableDefault(sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long viewerId = userDetails != null ? userDetails.getUser().getId() : null;

        Page<CommentDto> comments = devCommentService.getAllCommentsWithLikes(viewerId, recipeId, pageable);
        return ResponseEntity.ok(comments);
    }

    @GetMapping("/{commentId}/replies")
    @Operation(summary = "Dev V3 대댓글 목록",
            description = """
                    운영 `GET /api/recipes/{recipeId}/comments/{commentId}/replies` 미러. dev V3 차이점:
                      - **권한 게이트** 적용 (위 댓글 목록과 동일)
                      - 정렬: `createdAt` (default ASC) | `likeCount`
                      - parent 댓글 + page<reply>를 함께 반환
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공"),
            @ApiResponse(responseCode = "403", description = "PRIVATE/RESTRICTED non-owner", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피/댓글 없음 또는 non-ACTIVE", content = @Content)
    })
    public ResponseEntity<CommentWithRepliesDto> getCommentWithReplies(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @Parameter(description = "댓글 ID (HashID)") @DecodeId("commentId") Long commentId,
            @PageableDefault(sort = "createdAt", direction = Sort.Direction.ASC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long viewerId = userDetails != null ? userDetails.getUser().getId() : null;

        CommentWithRepliesDto result = devCommentService.getCommentWithReplies(viewerId, recipeId, commentId, pageable);
        return ResponseEntity.ok(result);
    }

    @DeleteMapping("/{commentId}")
    @Operation(summary = "Dev V3 댓글 삭제",
            description = """
                    운영 `DELETE /api/recipes/{recipeId}/comments/{commentId}` 미러. dev V3 차이점 없음 —
                    cleanup right이라 게이트 없음. 운영 service가 본인 댓글인지 확인 (COMMENT_ACCESS_DENIED).
                    URL의 recipeId는 RESTful 일관성용 — 실제 삭제는 commentId만 사용 (운영과 동일).
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "삭제 성공"),
            @ApiResponse(responseCode = "401", description = "인증 필요", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 댓글 아님 (COMMENT_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "댓글 없음 (COMMENT_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<String> deleteComment(
            @Parameter(description = "댓글 ID (HashID)") @DecodeId("commentId") Long commentId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        devCommentService.deleteComment(userId, commentId);
        return ResponseEntity.ok("댓글이 삭제되었습니다.");
    }
}
