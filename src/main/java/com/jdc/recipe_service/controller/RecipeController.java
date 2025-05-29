package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;
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

    @PostMapping
    public ResponseEntity<PresignedUrlResponse> createRecipeWithImages(
            @RequestParam(value = "source", required = false) String source,
            @RequestBody @Valid RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        RecipeSourceType sourceType = RecipeSourceType.fromNullable(source);

        if (sourceType == RecipeSourceType.AI) {
            request = recipeService.buildRecipeFromAiRequest(request);
        }
        PresignedUrlResponse response = recipeService.createUserRecipeAndGenerateUrls(
                request,
                userDetails.getUser().getId(),
                sourceType
        );

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

//    @PostMapping("/ai/generate")
//    public ResponseEntity<RecipeCreateRequestDto> previewAiRecipe(
//            @RequestBody @Valid AiRecipeRequestDto aiReq) {
//        RecipeCreateRequestDto generated = recipeService.generateAiRecipe(aiReq);
//        return ResponseEntity.ok(generated);
//    }

    @PutMapping("/{recipeId}")
    public ResponseEntity<PresignedUrlResponse> updateRecipe(
            @PathVariable Long recipeId,
            @RequestBody @Valid RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        PresignedUrlResponse response = recipeService.updateUserRecipe(
                recipeId,
                userDetails.getUser().getId(),
                request
        );

        return ResponseEntity.ok(response);
    }


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
