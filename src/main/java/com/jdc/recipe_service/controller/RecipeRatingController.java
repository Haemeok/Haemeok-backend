package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingRequestDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeRatingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/ratings/recipe")
@RequiredArgsConstructor
@Tag(name = "레시피 평점 API", description = "레시피에 대한 평점 등록, 조회, 삭제 기능을 제공합니다.")
public class RecipeRatingController {

    private final RecipeRatingService recipeRatingService;

    @PostMapping("/{id}")
    @Operation(summary = "레시피 평점 등록/수정", description = "사용자가 레시피에 평점을 등록하거나 기존 평점을 수정합니다.")
    public ResponseEntity<?> rateRecipe(
            @Parameter(description = "레시피 ID") @PathVariable Long id,
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "평점 요청 DTO") @RequestBody RecipeRatingRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        recipeRatingService.rateRecipe(id, userId, dto);

        return ResponseEntity.ok(Map.of("message", "평점 등록 완료"));
    }

    @GetMapping("/{id}/me")
    @Operation(summary = "내 평점 조회", description = "현재 로그인한 사용자가 지정한 레시피에 준 평점을 조회합니다.")
    public ResponseEntity<?> getMyRating(
            @Parameter(description = "레시피 ID") @PathVariable Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        Double rating = recipeRatingService.getMyRating(id, userId);
        return ResponseEntity.ok(Map.of("rating", rating));
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "평점 삭제", description = "사용자가 지정한 레시피에 등록한 평점을 삭제합니다.")
    public ResponseEntity<?> deleteRating(
            @Parameter(description = "레시피 ID") @PathVariable Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        recipeRatingService.deleteRating(id, userId);
        return ResponseEntity.ok(Map.of("message", "평점 삭제 완료"));
    }
}
