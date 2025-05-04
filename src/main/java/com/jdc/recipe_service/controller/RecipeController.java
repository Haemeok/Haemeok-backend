package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
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
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
public class RecipeController {

    private final RecipeService recipeService;

    // 레시피 생성 + 이미지 Presigned URL 발급
    @PostMapping
    public ResponseEntity<PresignedUrlResponse> createRecipeWithImages(
            @RequestParam(value = "source", required = false) String source,
            @RequestBody @Valid RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        RecipeSourceType sourceType = RecipeSourceType.fromNullable(source);
        PresignedUrlResponse response = recipeService.createUserRecipeAndGenerateUrls(
                request,
                userDetails.getUser().getId(),
                sourceType
        );

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    // 레시피 수정
    @PutMapping("/{recipeId}")
    public ResponseEntity<Map<String, Long>> updateRecipe(
            @PathVariable Long recipeId,
            @RequestBody @Valid RecipeCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long updatedId = recipeService.updateUserRecipe(recipeId, userDetails.getUser().getId(), dto);
        return ResponseEntity.ok(Map.of("recipeId", updatedId));
    }

    // 레시피 삭제
    @DeleteMapping("/{recipeId}")
    public ResponseEntity<String> deleteRecipe(
            @PathVariable Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        recipeService.deleteRecipe(recipeId, userDetails.getUser().getId());
        return ResponseEntity.ok("레시피가 성공적으로 삭제되었습니다.");
    }

    //공개, 비공개
    @PostMapping("/{recipeId}/private")
    public ResponseEntity<?> togglePrivacy(
            @PathVariable Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();
        boolean newIsPrivate = recipeService.togglePrivacy(recipeId, userId);

        return ResponseEntity.ok(Map.of(
                "isPrivate", newIsPrivate,
                "message", newIsPrivate ? "레시피가 비공개로 전환되었습니다." : "레시피가 공개로 전환되었습니다."
        ));
    }


}
