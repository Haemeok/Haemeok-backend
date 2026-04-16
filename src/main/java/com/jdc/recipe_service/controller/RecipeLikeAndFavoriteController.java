package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeBookService;
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
    private final RecipeBookService recipeBookService;

    @PostMapping("/{id}/like")
    @Operation(summary = "레시피 좋아요 토글", description = "레시피에 좋아요를 등록하거나 취소합니다. 이미 좋아요 상태면 취소되고, 아니라면 등록됩니다.")
    public ResponseEntity<?> toggleLike(
            @Parameter(description = "레시피 ID") @DecodeId Long id,
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
    @Operation(summary = "레시피 저장 토글", description = "레시피를 저장하거나 저장 해제합니다. 저장 시 기본 폴더에 추가되며, 해제 시 모든 폴더에서 제거됩니다.")
    public ResponseEntity<?> toggleFavorite(
            @Parameter(description = "레시피 ID") @DecodeId Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        boolean saved = recipeBookService.toggleSave(userId, id);
        return ResponseEntity.ok(Map.of(
                "saved", saved,
                "message", saved ? "레시피 저장 완료" : "레시피 저장 해제 완료"
        ));
    }
}