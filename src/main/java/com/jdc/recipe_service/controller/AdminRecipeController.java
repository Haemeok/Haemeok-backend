package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.report.AdminIngredientUpdateDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.AdminRecipeService;
import com.jdc.recipe_service.service.image.RecipeImageService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/admin/recipes")
@RequiredArgsConstructor
@PreAuthorize("hasRole('ADMIN')")
@Tag(name = "관리자 전용 레시피 API", description = "크롤링된 레시피를 등록, 수정, 삭제하는 관리자 전용 API입니다.")
public class AdminRecipeController {

    private final AdminRecipeService recipeService;
    private final Hashids hashids;
    private final RecipeImageService recipeImageService;

    @PostMapping
    @Operation(summary = "크롤링 레시피 단건 등록", description = "관리자가 단일 크롤링 레시피를 저장합니다.")
    public ResponseEntity<Map<String, String>> createCrawledRecipe(
            @RequestBody @Valid RecipeCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = userDetails.getUser().getId();
        Long recipeId = recipeService.createRecipe(dto, userId);
        return ResponseEntity.ok(Map.of("recipeId", hashids.encode(recipeId)));
    }

    @PostMapping("/bulk")
    @Operation(summary = "크롤링 레시피 일괄 등록", description = "관리자가 여러 개의 크롤링 레시피를 한 번에 저장합니다.")
    public ResponseEntity<Map<String, Object>> createCrawledRecipesInBulk(
            @RequestBody @Valid List<RecipeCreateRequestDto> recipes,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = userDetails.getUser().getId();
        List<Long> createdIds = recipeService.createRecipesInBulk(recipes, userId);

        List<String> encodedIds = createdIds.stream()
                .map(hashids::encode)
                .collect(Collectors.toList());

        return ResponseEntity.ok(Map.of(
                "status", "success",
                "createdCount", createdIds.size(),
                "recipeIds", encodedIds
        ));
    }

    @PostMapping("/with-images")
    @Operation(summary = "크롤링 레시피 + 이미지 업로드 URL 발급", description = "레시피와 함께 이미지 업로드용 Presigned URL을 발급합니다.")
    public ResponseEntity<PresignedUrlResponse> createCrawledRecipeWithPresignedUrls(
            @RequestBody RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = userDetails.getUser().getId();
        PresignedUrlResponse response = recipeService.createRecipeAndPresignedUrls(request, userId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{recipeId}")
    @Operation(summary = "크롤링 레시피 수정", description = "관리자가 기존의 크롤링 레시피를 수정합니다.")
    public ResponseEntity<Map<String, String>> updateCrawledRecipe(
            @Parameter(description = "레시피 ID") @DecodeId Long recipeId,
            @RequestBody RecipeCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = userDetails.getUser().getId();
        Long updatedId = recipeService.updateRecipe(recipeId, userId, dto);
        return ResponseEntity.ok(Map.of("recipeId", hashids.encode(updatedId)));    }

    @DeleteMapping("/{recipeId}")
    @Operation(summary = "크롤링 레시피 삭제", description = "관리자가 특정 크롤링 레시피를 삭제합니다.")
    public ResponseEntity<String> deleteCrawledRecipe(
            @Parameter(description = "레시피 ID") @DecodeId Long recipeId,
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

    @PutMapping("/{recipeId}/ingredients")
    @Operation(summary = "재료 일괄 수정 (관리자)", description = "관리자가 재료를 추가, 수정, 삭제하고 신고 내역을 자동 해결 처리합니다.")
    public ResponseEntity<String> updateIngredientsBatch(
            @Parameter(description = "레시피 ID") @DecodeId Long recipeId,
            @RequestBody List<AdminIngredientUpdateDto> dtos,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null || userDetails.getUser().getRole() != Role.ADMIN) {
            throw new CustomException(ErrorCode.ADMIN_ACCESS_DENIED);
        }

        recipeService.updateIngredientsBatch(recipeId, dtos);

        return ResponseEntity.ok("재료 수정 및 관련 신고 처리가 완료되었습니다.");
    }

    @PostMapping("/{recipeId}/regenerate-image")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<String> regenerateRecipeImage(@DecodeId Long recipeId) {
        recipeImageService.regenerateAndApplyImage(recipeId);
        return ResponseEntity.ok("이미지 재생성 작업이 백그라운드에서 시작되었습니다.");
    }
}