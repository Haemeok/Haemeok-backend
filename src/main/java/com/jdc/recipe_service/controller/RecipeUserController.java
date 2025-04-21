package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.user.RecipeUserCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.user.RecipeWithImageUserUploadRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Map;

@RestController
@RequestMapping("/api/recipes/user")
@RequiredArgsConstructor
public class RecipeUserController {

    private final RecipeService recipeService;

    // 유저 전용 레시피 생성(인증 필수)
    @PostMapping
    public ResponseEntity<?> createUserRecipe(
            @RequestBody @Valid RecipeUserCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        Long recipeId = recipeService.createUserRecipe(dto, userId);
        return ResponseEntity.ok(Map.of("recipeId", recipeId));
    }

    // 레시피 + 이미지 생성
    @PostMapping("/with-images")
    public ResponseEntity<PresignedUrlResponse> createRecipeWithPresignedUrls(
            @RequestBody RecipeWithImageUserUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            System.out.println("❌ userDetails is null"); // or log.warn
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        System.out.println("✅ 인증된 사용자: " + userDetails.getUsername());
        Long userId = userDetails.getUser().getId();
        PresignedUrlResponse response = recipeService.createUserRecipeAndGenerateUrls(request, userId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    // 유저 전용 레시피 수정(인증 필수)
    @PutMapping("/{id}")
    public ResponseEntity<?> updateUserRecipe(
            @PathVariable Long id,
            @RequestBody RecipeUserCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        recipeService.updateUserRecipe(id, userId, dto);
        return ResponseEntity.ok(Map.of("recipeId", id));
    }

}