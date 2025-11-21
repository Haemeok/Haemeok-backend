package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateWithImageRequest;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.domain.type.RobotType;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.ai.RecipeAnalysisService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "레시피 관리 API", description = "레시피 생성, 수정, 삭제 및 비공개 설정을 위한 API입니다.")
public class RecipeController {

    private final RecipeService recipeService;
    private final RecipeAnalysisService recipeAnalysisService;

    @PostMapping
    @Operation(summary = "레시피 생성 + 이미지 Presigned URL 발급", description = "레시피 생성 요청과 함께 이미지 업로드용 Presigned URL을 발급합니다. source 값에 따라 AI 생성 또는 유저 입력을 구분합니다.")
    public ResponseEntity<PresignedUrlResponse> createRecipeWithImages(
            @Parameter(description = "레시피 생성 출처 (예: AI, USER)") @RequestParam(value = "source", required = false) String source,
            @Parameter(description = "AI 레시피 생성 시 사용할 로봇 타입 (예: CLASSIC, FUNNY 등)") @RequestParam(value = "robotType", required = false) RobotType robotTypeParam,
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "레시피 생성 요청 DTO (이미지 키 포함)") @RequestBody @Valid RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        RecipeSourceType sourceType = RecipeSourceType.fromNullable(source);

        Long userId = userDetails.getUser().getId();
        PresignedUrlResponse response;

        if (sourceType == RecipeSourceType.AI) {
            response = recipeService.createRecipeWithAiLogic(
                    sourceType,
                    robotTypeParam,
                    request,
                    userId
            );
        } else {
            response = recipeService.createUserRecipeAndGenerateUrls(request, userId, sourceType);
        }
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{recipeId}")
    @Operation(summary = "레시피 수정", description = "기존 레시피를 수정하고 이미지가 변경된 경우 Presigned URL을 다시 발급합니다.")
    public ResponseEntity<PresignedUrlResponse> updateRecipe(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "수정할 레시피 정보")
            @RequestBody @Valid RecipeUpdateWithImageRequest request,
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
    @Operation(summary = "레시피 삭제", description = "지정한 레시피를 삭제합니다. 작성자 본인만 삭제할 수 있습니다.")
    public ResponseEntity<String> deleteRecipe(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        recipeService.deleteRecipe(recipeId, userDetails.getUser().getId());
        return ResponseEntity.ok("레시피가 성공적으로 삭제되었습니다.");
    }

    @PostMapping("/{recipeId}/private")
    @Operation(summary = "레시피 공개/비공개 전환", description = "레시피의 공개 여부를 토글합니다. 공개 → 비공개 또는 비공개 → 공개로 전환됩니다.")
    public ResponseEntity<?> togglePrivacy(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
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

    @PostMapping("/{recipeId}/analyze")
    @Operation(summary = "레시피 AI 분석 수동 요청 (관리자 전용)", description = "특정 레시피의 가격 책정, 팁 생성, 유해성 검사를 실행합니다.")
    public ResponseEntity<String> analyzeRecipeManually(
            @Parameter(description = "레시피 ID") @PathVariable Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        if (userDetails.getUser().getRole() != Role.ADMIN) {
            throw new CustomException(ErrorCode.ADMIN_ACCESS_DENIED);
        }

        recipeAnalysisService.analyzeRecipeAsync(recipeId);

        return ResponseEntity.ok("AI 분석 요청이 비동기로 전송되었습니다. 잠시 후 결과가 반영됩니다.");
    }
}