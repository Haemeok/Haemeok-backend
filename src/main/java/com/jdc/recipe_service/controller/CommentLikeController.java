package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CommentLikeService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/comments")
@RequiredArgsConstructor
public class CommentLikeController {

    private final CommentLikeService commentLikeService;

    // 댓글 좋아요 토글
    @PostMapping("/{commentId}/like")
    public ResponseEntity<?> toggleLike(
            @PathVariable Long commentId,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED)
                    .body(Map.of("message", "로그인이 필요합니다."));
        }
        Long userId = userDetails.getUser().getId();
        boolean liked = commentLikeService.toggleLike(commentId, userId);
        String message = liked ? "댓글 좋아요 등록 완료" : "댓글 좋아요 취소 완료";
        return ResponseEntity.ok(Map.of(
                "liked", liked,
                "message", message
        ));
    }
}
