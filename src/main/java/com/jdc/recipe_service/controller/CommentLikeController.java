package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CommentLikeService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/comments")
@RequiredArgsConstructor
@Tag(name = "댓글 좋아요 API", description = "댓글에 대한 좋아요 등록 및 취소 기능을 제공합니다.")
public class CommentLikeController {

    private final CommentLikeService commentLikeService;

    @PostMapping("/{commentId}/like")
    @Operation(summary = "댓글 좋아요 토글", description = "댓글에 좋아요를 누르거나 취소합니다. 결과로 liked 상태와 현재 좋아요 수를 반환합니다.")
    public ResponseEntity<?> toggleLike(
            @Parameter(description = "댓글 ID") @DecodeId Long commentId,
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