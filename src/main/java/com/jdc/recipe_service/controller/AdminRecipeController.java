package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.AdminRecipeService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/admin/recipes")
@RequiredArgsConstructor
@PreAuthorize("hasRole('ADMIN')")
public class AdminRecipeController {

    private final AdminRecipeService recipeService;

    // 크롤링 레시피 생성
    @PostMapping
    public ResponseEntity<Map<String, Long>> createCrawledRecipe(
            @RequestBody @Valid RecipeCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = userDetails.getUser().getId();
        Long recipeId = recipeService.createRecipe(dto, userId);
        return ResponseEntity.ok(Map.of("recipeId", recipeId));
    }

    @PostMapping("/bulk")
    public ResponseEntity<Map<String, Object>> createCrawledRecipesInBulk(
            @RequestBody @Valid List<RecipeCreateRequestDto> recipes,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = userDetails.getUser().getId();
        List<Long> createdIds = recipeService.createRecipesInBulk(recipes, userId);

        return ResponseEntity.ok(Map.of(
                "status", "success",
                "createdCount", createdIds.size(),
                "recipeIds", createdIds
        ));
    }

    // 크롤링 + 이미지 Presigned URL 생성
    @PostMapping("/with-images")
    public ResponseEntity<PresignedUrlResponse> createCrawledRecipeWithPresignedUrls(
            @RequestBody RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = userDetails.getUser().getId();
        PresignedUrlResponse response = recipeService.createRecipeAndPresignedUrls(request, userId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    // 크롤링 레시피 수정
    @PutMapping("/{recipeId}")
    public ResponseEntity<Map<String, Long>> updateCrawledRecipe(
            @PathVariable Long recipeId,
            @RequestBody RecipeCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = userDetails.getUser().getId();
        Long updatedId = recipeService.updateRecipe(recipeId, userId, dto);
        return ResponseEntity.ok(Map.of("recipeId", updatedId));
    }

    // 크롤링 레시피 삭제
    @DeleteMapping("/{recipeId}")
    public ResponseEntity<String> deleteCrawledRecipe(
            @PathVariable Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        boolean isAdmin = userDetails.getAuthorities().stream()
                .anyMatch(auth -> auth.getAuthority().equals("ROLE_ADMIN"));
        recipeService.deleteRecipe(recipeId, userId, isAdmin);
        return ResponseEntity.ok("크롤링 레시피가 성공적으로 삭제되었습니다.");
    }
}
