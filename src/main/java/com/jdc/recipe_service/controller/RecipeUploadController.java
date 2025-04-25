package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.RecipeImageKeyUpdateRequest;
import com.jdc.recipe_service.domain.dto.url.FinalizeResponse;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlResponse;
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
public class RecipeUploadController {

    private final RecipeUploadService recipeUploadService;
    private final RecipeService recipeService;

    // 이미지만 수정
    @PutMapping("/{id}/images")
    public ResponseEntity<Void> updateRecipeImageKeys(
            @PathVariable Long id,
            @RequestBody RecipeImageKeyUpdateRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        recipeService.updateImageKeys(id, userDetails.getUser().getId(), request);
        return ResponseEntity.ok().build();
    }

    // 이미지 업로드용 Presigned URL 요청 (레시피 수정 시 사용)
    @PostMapping("/{id}/presigned-urls")
    public ResponseEntity<UpdatePresignedUrlResponse> getPresignedUrlsForUpdate(
            @PathVariable Long id,
            @RequestBody UpdatePresignedUrlRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails.getUser().getId();
        UpdatePresignedUrlResponse response = recipeUploadService.generatePresignedUrlsForUpdate(id, userId, request.getFiles());
        return ResponseEntity.ok(response);
    }

    @PostMapping("/{id}/finalize")
    public ResponseEntity<FinalizeResponse> finalizeRecipeImages(@PathVariable Long id) {
        FinalizeResponse response = recipeService.finalizeRecipeImages(id);
        return ResponseEntity.ok(response);
    }

}

