package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.RecipeImageKeyUpdateRequest;
import com.jdc.recipe_service.domain.dto.url.FinalizeResponse;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlResponse;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.RecipeUploadService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
public class RecipeImageUploadController {

    private final RecipeUploadService recipeUploadService;
    private final RecipeService recipeService;

    // 이미지만 수정
    @PutMapping("/{recipeId}/images")
    public ResponseEntity<Void> updateRecipeImageKeys(
            @PathVariable Long recipeId,
            @RequestBody RecipeImageKeyUpdateRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        recipeService.updateImageKeys(recipeId, userDetails.getUser().getId(), request);
        return ResponseEntity.ok().build();
    }

    // 이미지 업로드용 Presigned URL 요청 (레시피 수정 시 사용)
    @PostMapping("/{recipeId}/presigned-urls")
    public ResponseEntity<UpdatePresignedUrlResponse> getPresignedUrlsForUpdate(
            @PathVariable Long recipeId,
            @RequestBody UpdatePresignedUrlRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails.getUser().getId();
        UpdatePresignedUrlResponse response = recipeUploadService.generatePresignedUrlsForUpdate(recipeId, userId, request.getFiles());
        return ResponseEntity.ok(response);
    }

    @PostMapping("/{recipeId}/finalize")
    public ResponseEntity<FinalizeResponse> finalizeRecipeImages(
            @PathVariable Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();
        boolean isAdmin = userDetails.getAuthorities().stream()
                .anyMatch(auth -> auth.getAuthority().equals("ROLE_ADMIN"));

        FinalizeResponse response = recipeService.finalizeRecipeImages(recipeId, userId, isAdmin);
        return ResponseEntity.ok(response);
    }


}

