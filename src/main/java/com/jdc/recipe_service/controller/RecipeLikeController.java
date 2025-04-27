package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeFavoriteService;
import com.jdc.recipe_service.service.RecipeLikeService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/recipes")
public class RecipeLikeController {

    private final RecipeLikeService likeService;
    private final RecipeFavoriteService favoriteService;

    @PostMapping("/{id}/like")
    public ResponseEntity<?> toggleLike(
            @PathVariable Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        boolean liked = likeService.toggleLike(userId, id);
        return ResponseEntity.ok(Map.of(
                "liked", liked,
                "message", liked ? "레시피 좋아요 등록 완료" : "레시피 좋아요 취소 완료"
        ));
    }

    @PostMapping("/{id}/favorite")
    public ResponseEntity<?> toggleFavorite(
            @PathVariable Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        boolean favorited = favoriteService.toggleFavorite(userId, id);
        return ResponseEntity.ok(Map.of(
                "favorited", favorited,
                "message", favorited ? "레시피 즐겨찾기 등록 완료" : "레시피 즐겨찾기 취소 완료"
        ));
    }
}
