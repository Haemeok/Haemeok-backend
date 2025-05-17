package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CommentService;
import jakarta.validation.Valid;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping("/api/recipes/{recipeId}/comments")
@RequiredArgsConstructor
public class CommentController {

    private final CommentService commentService;

    // 1) 댓글 생성 (상위 댓글)
    @PostMapping
    public ResponseEntity<CommentDto> createComment(
            @PathVariable Long recipeId,
            @Valid @RequestBody CommentRequestDto requestDto,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        CommentDto created = commentService.createComment(
                recipeId, requestDto, userDetails.getUser());
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    // 2) 대댓글 생성
    @PostMapping("/{parentId}/replies")
    public ResponseEntity<CommentDto> createReply(
            @PathVariable Long recipeId,
            @PathVariable Long parentId,
            @Valid @RequestBody CommentRequestDto requestDto,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        CommentDto created = commentService.createReply(
                recipeId, parentId, requestDto, userDetails.getUser().getId());
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    // 3) 댓글 조회 (좋아요 포함)
    @GetMapping
    public ResponseEntity<Page<CommentDto>> getAllCommentsWithLikes(
            @PathVariable Long recipeId,
            @PageableDefault(sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;
        Page<CommentDto> comments = commentService.getAllCommentsWithLikes(recipeId, userId, pageable);
        return ResponseEntity.ok(comments);
    }

    // 4) 대댓글 조회
    @GetMapping("/{parentId}/replies")
    public ResponseEntity<Page<CommentDto>> getReplies(
            @PathVariable Long parentId,
            @PageableDefault(sort = "createdAt", direction = Sort.Direction.ASC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;
        Page<CommentDto> replies = commentService.getRepliesWithLikes(parentId, userId, pageable);
        return ResponseEntity.ok(replies);
    }


    // 5) 댓글 삭제
    @DeleteMapping("/{commentId}")
    public ResponseEntity<String> deleteComment(
            @PathVariable Long commentId,
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
