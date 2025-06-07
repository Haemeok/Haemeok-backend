package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeFavoriteService;
import com.jdc.recipe_service.service.RecipeLikeService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/recipes")
@Tag(name = "레시피 좋아요/즐겨찾기 API", description = "레시피에 대한 좋아요 및 즐겨찾기 토글 기능을 제공합니다.")
public class RecipeLikeAndFavoriteController {

    private final RecipeLikeService likeService;
    private final RecipeFavoriteService favoriteService;

    @PostMapping("/{id}/like")
    @Operation(summary = "레시피 좋아요 토글", description = "레시피에 좋아요를 등록하거나 취소합니다. 이미 좋아요 상태면 취소되고, 아니라면 등록됩니다.")
    public ResponseEntity<?> toggleLike(
            @Parameter(description = "레시피 ID") @PathVariable Long id,
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
    @Operation(summary = "레시피 즐겨찾기 토글", description = "레시피를 즐겨찾기에 등록하거나 해제합니다. 이미 즐겨찾기 상태면 해제되고, 아니라면 등록됩니다.")
    public ResponseEntity<?> toggleFavorite(
            @Parameter(description = "레시피 ID") @PathVariable Long id,
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