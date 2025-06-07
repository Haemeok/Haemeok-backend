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
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
@Tag(name = "레시피 이미지 업로드 API", description = "레시피 이미지 키 업데이트, Presigned URL 발급, 이미지 업로드 완료 처리 등을 담당합니다.")
public class RecipeImageUploadController {

    private final RecipeUploadService recipeUploadService;
    private final RecipeService recipeService;

    @PutMapping("/{recipeId}/images")
    @Operation(summary = "레시피 이미지 키 업데이트", description = "업로드된 이미지의 S3 키 정보를 레시피에 반영합니다.")
    public ResponseEntity<Void> updateRecipeImageKeys(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "이미지 키 리스트 (main, steps 등 포함)") @RequestBody RecipeImageKeyUpdateRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        recipeService.updateImageKeys(recipeId, userDetails.getUser().getId(), request);
        return ResponseEntity.ok().build();
    }

    @PostMapping("/{recipeId}/presigned-urls")
    @Operation(summary = "레시피 이미지 Presigned URL 발급", description = "레시피 수정 시 필요한 이미지 업로드용 Presigned URL을 발급합니다.")
    public ResponseEntity<UpdatePresignedUrlResponse> getPresignedUrlsForUpdate(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "업로드할 파일 목록") @RequestBody UpdatePresignedUrlRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails.getUser().getId();
        UpdatePresignedUrlResponse response = recipeUploadService.generatePresignedUrlsForUpdate(recipeId, userId, request.getFiles());
        return ResponseEntity.ok(response);
    }

    @PostMapping("/{recipeId}/finalize")
    @Operation(summary = "레시피 이미지 업로드 완료 처리", description = "레시피 이미지 업로드가 완료되었음을 백엔드에 알려 색인 및 상태 갱신 처리를 수행합니다. 관리자 계정 여부도 판단합니다.")
    public ResponseEntity<FinalizeResponse> finalizeRecipeImages(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
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
