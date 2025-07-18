package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.domain.dto.comment.CommentWithRepliesDto;
import com.jdc.recipe_service.domain.dto.comment.ReplyDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CommentService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
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
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/recipes/{recipeId}/comments")
@RequiredArgsConstructor
@Tag(name = "레시피 댓글 API", description = "레시피에 대한 댓글 및 대댓글 작성, 조회, 삭제 기능을 제공합니다.")
public class CommentController {

    private final CommentService commentService;

    @PostMapping
    @Operation(summary = "댓글 작성", description = "레시피에 새로운 댓글을 작성합니다.")
    public ResponseEntity<CommentDto> createComment(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "댓글 내용") @Valid @RequestBody CommentRequestDto requestDto,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        CommentDto created = commentService.createComment(
                recipeId, requestDto, userDetails.getUser());
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @PostMapping("/{parentId}/replies")
    @Operation(summary = "대댓글 작성", description = "기존 댓글에 대한 답글(대댓글)을 작성합니다.")
    public ResponseEntity<ReplyDto> createReply(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
            @Parameter(description = "부모 댓글 ID") @PathVariable Long parentId,
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "대댓글 내용") @Valid @RequestBody CommentRequestDto requestDto,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        ReplyDto created = commentService.createReply(
                recipeId, parentId, requestDto, userDetails.getUser());
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @GetMapping
    @Operation(summary = "댓글 목록 조회", description = "레시피에 작성된 상위 댓글 목록을 페이징하여 조회합니다. 정렬 기준: createdAt, likeCount")
    public ResponseEntity<Page<CommentDto>> getAllCommentsWithLikes(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
            @Parameter(
                    name = "sort",
                    description = "정렬 기준 (예: createdAt,DESC 또는 likeCount,DESC)",
                    example = "createdAt,DESC"
            )
            @PageableDefault(sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;
        Page<CommentDto> comments = commentService.getAllCommentsWithLikes(recipeId, userId, pageable);
        return ResponseEntity.ok(comments);
    }

    @GetMapping("/{commentId}/replies")
    @Operation(summary = "대댓글 목록 조회", description = "특정 댓글의 대댓글 목록을 페이징하여 조회합니다. 정렬 기준: createdAt, likeCount")
    public ResponseEntity<CommentWithRepliesDto> getCommentWithReplies(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
            @Parameter(description = "댓글 ID") @PathVariable Long commentId,
            @Parameter(
                    name = "sort",
                    description = "정렬 기준 (예: createdAt,ASC 또는 likeCount,DESC)",
                    example = "createdAt,ASC"
            )
            @PageableDefault(sort = "createdAt", direction = Sort.Direction.ASC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long currentUserId = userDetails != null
                ? userDetails.getUser().getId()
                : null;

        CommentDto parent = commentService.findByIdAndRecipeId(commentId, recipeId, currentUserId);
        Page<ReplyDto> page = commentService.getRepliesWithLikes(commentId, currentUserId, pageable);

        return ResponseEntity.ok(new CommentWithRepliesDto(parent, page));
    }

    @DeleteMapping("/{commentId}")
    @Operation(summary = "댓글 삭제", description = "지정한 댓글을 삭제합니다. 본인만 삭제할 수 있습니다.")
    public ResponseEntity<String> deleteComment(
            @Parameter(description = "댓글 ID") @PathVariable Long commentId,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        commentService.deleteComment(commentId, userId);
        return ResponseEntity.ok("댓글이 삭제되었습니다.");
    }
}