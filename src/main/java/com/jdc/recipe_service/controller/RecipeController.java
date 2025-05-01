package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.dto.url.FinalizeResponse;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CookingRecordService;
import com.jdc.recipe_service.service.RecipeService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
public class RecipeController {

    private final RecipeService recipeService;
    private final CookingRecordService cookingRecordService;

    // 1) 레시피 생성 (인증 필수)
    @PostMapping
    public ResponseEntity<Long> createRecipe(
            @RequestBody @Valid RecipeCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        return ResponseEntity.ok(recipeService.createRecipe(dto, userId));
    }

    // 레시피 + 이미지 생성
    @PostMapping("/with-images")
    public ResponseEntity<PresignedUrlResponse> createRecipeWithPresignedUrls(
            @RequestBody RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        PresignedUrlResponse response = recipeService.createRecipeAndPresignedUrls(request, userId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    // 3) 레시피 수정 (인증 필수)
    @PutMapping("/{recipeId}")
    public ResponseEntity<Map<String, Long>> updateRecipe(
            @PathVariable Long recipeId,
            @RequestBody RecipeCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        Long updatedId = recipeService.updateRecipe(recipeId, userId, dto);
        return ResponseEntity.ok(Map.of("id", updatedId));
    }

    // 4) 레시피 삭제 (인증 필수)
    @DeleteMapping("/{recipeId}")
    public ResponseEntity<String> deleteRecipe(
            @PathVariable Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        recipeService.deleteRecipe(recipeId, userId);
        cookingRecordService.deleteByRecipeId(recipeId);
        return ResponseEntity.ok("레시피가 성공적으로 삭제되었습니다.");
    }

}