package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.exception.CommentAccessDeniedException;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CommentService;
import org.springframework.http.HttpStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/recipes/{recipeId}/comments")
@RequiredArgsConstructor
public class CommentController {

    private final CommentService commentService;

    // 1) 댓글 생성 (상위 댓글)
    @PostMapping
    public ResponseEntity<CommentDto> createComment(
            @PathVariable Long recipeId,
            @RequestBody CommentRequestDto requestDto,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        CommentDto created = commentService.createComment(
                recipeId, requestDto, userDetails.getUser());
        // REST 관례에 따라 201 Created
        return ResponseEntity.status(HttpStatus.CREATED).body(created);

    }

    // 2) 대댓글 생성
    @PostMapping("/{parentId}/replies")
    public ResponseEntity<CommentDto> createReply(
            @PathVariable Long recipeId,
            @PathVariable Long parentId,
            @RequestBody CommentRequestDto requestDto,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        CommentDto created = commentService.createReply(
                recipeId, parentId, requestDto, userDetails.getUser().getId());
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    // 3) 댓글 조회 (좋아요 포함)
    @GetMapping
    public ResponseEntity<List<CommentDto>> getAllCommentsWithLikes(
            @PathVariable Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;
        List<CommentDto> comments = commentService.getAllCommentsWithLikes(recipeId, userId);
        return ResponseEntity.ok(comments);
    }

    // 4) 대댓글 조회
    @GetMapping("/{parentId}/replies")
    public ResponseEntity<List<CommentDto>> getReplies(
            @PathVariable Long recipeId,
            @PathVariable Long parentId,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;
        List<CommentDto> replies = commentService.getRepliesWithLikes(parentId, userId);
        return ResponseEntity.ok(replies);
    }

    // 5) 댓글 삭제
    @DeleteMapping("/{commentId}")
    public ResponseEntity<String> deleteComment(
            @PathVariable Long commentId,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        try {
            commentService.deleteComment(commentId, userId);
            return ResponseEntity.ok("댓글이 삭제되었습니다.");
        } catch (CommentAccessDeniedException ex) {
            return ResponseEntity.status(HttpStatus.FORBIDDEN)
                    .body(ex.getMessage());
        }
    }
}
