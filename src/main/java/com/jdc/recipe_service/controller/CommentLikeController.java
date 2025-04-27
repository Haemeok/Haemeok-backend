package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CommentLikeService;
import lombok.RequiredArgsConstructor;
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
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        boolean liked = commentLikeService.toggleLike(commentId, userId);
        int likeCount = commentLikeService.countLikes(commentId);

        return ResponseEntity.ok(Map.of(
                "liked", liked,
                "likeCount", likeCount,
                "message", liked ? "댓글 좋아요 등록 완료" : "댓글 좋아요 취소 완료"
        ));
    }

}
