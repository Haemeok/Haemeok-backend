package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingResponseDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CookingRecordService;
import com.jdc.recipe_service.service.RecipeRatingService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/ratings/recipe")
@RequiredArgsConstructor
public class RecipeRatingController {

    private final RecipeRatingService recipeRatingService;

    // 평점 CRUD (인증 필수)
    @PostMapping("/{id}")
    public ResponseEntity<?> rateRecipe(
            @PathVariable Long id,
            @RequestBody RecipeRatingRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        recipeRatingService.rateRecipe(id, userId, dto);

        return ResponseEntity.ok(Map.of("message", "평점 등록 완료"));
    }

    @GetMapping("/{id}/me")
    public ResponseEntity<?> getMyRating(
            @PathVariable Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        Double rating = recipeRatingService.getMyRating(id, userId);
        return ResponseEntity.ok(Map.of("rating", rating));
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<?> deleteRating(
            @PathVariable Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        recipeRatingService.deleteRating(id, userId);
        return ResponseEntity.ok(Map.of("message", "평점 삭제 완료"));
    }
}
