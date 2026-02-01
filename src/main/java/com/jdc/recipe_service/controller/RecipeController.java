package com.jdc.recipe_service.controller;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
import com.jdc.recipe_service.domain.type.Role;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeExtractionService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.ai.RecipeAnalysisService;
import com.jdc.recipe_service.service.media.YtDlpService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@RestController
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
@Tag(name = "레시피 관리 API", description = "레시피 생성, 수정, 삭제 및 비공개 설정을 위한 API입니다.")
public class RecipeController {

    private final RecipeService recipeService;
    private final RecipeAnalysisService recipeAnalysisService;
    private final RecipeExtractionService recipeExtractionService;

    @PostMapping
    @Operation(summary = "레시피 직접 등록 + 이미지 Presigned URL 발급", description = "사용자가 직접 입력한 레시피 정보를 저장하고, 이미지를 업로드할 Presigned URL을 발급합니다.")
    public ResponseEntity<PresignedUrlResponse> createRecipeWithImages(
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "레시피 생성 요청 DTO (이미지 키 포함)")
            @RequestBody @Valid RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        PresignedUrlResponse response = recipeService.createRecipeAndGenerateUrls(
                request,
                userDetails.getUser().getId(),
                RecipeSourceType.USER,
                null
        );

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{recipeId}")
    @Operation(summary = "레시피 수정", description = "기존 레시피를 수정하고 이미지가 변경된 경우 Presigned URL을 다시 발급합니다.")
    public ResponseEntity<PresignedUrlResponse> updateRecipe(
            @Parameter(description = "레시피 ID") @DecodeId Long recipeId,
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
            @Parameter(description = "레시피 ID") @DecodeId Long recipeId,
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
            @Parameter(description = "레시피 ID") @DecodeId Long recipeId,
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
            @Parameter(description = "레시피 ID") @DecodeId Long recipeId,
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

    @PostMapping("/{recipeId}/nutrition")
    @Operation(summary = "레시피 영양소 재계산 (관리자 전용)", description = "재료 정보를 바탕으로 탄단지/당/나트륨/칼로리를 다시 계산하여 DB에 반영합니다.")
    public ResponseEntity<String> recalculateNutrition(
            @Parameter(description = "레시피 ID") @DecodeId Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        if (userDetails.getUser().getRole() != Role.ADMIN) {
            throw new CustomException(ErrorCode.ADMIN_ACCESS_DENIED);
        }

        recipeService.recalculateNutrition(recipeId);

        return ResponseEntity.ok("영양소 정보가 성공적으로 재계산되어 DB에 반영되었습니다.");
    }

    @PostMapping("/extract")
    @Operation(summary = "유튜브 링크로 레시피 AI 추출", description = "유튜브 영상 URL을 분석하여 레시피를 자동 생성하고 저장합니다. (비동기 처리)")
    public DeferredResult<ResponseEntity<PresignedUrlResponse>> extractRecipeFromYoutube(
            @Parameter(description = "유튜브 영상 URL", required = true) @RequestParam String url,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        return recipeExtractionService.extractAndCreateRecipe(
                url,
                userDetails.getUser().getId(),
                userDetails.getUser().getNickname()
        );
    }

    @PostMapping("/extract/v2")
    @Operation(summary = "[V2] 유튜브 추출 요청", description = "유튜브 링크 백그라운드 추출 테스트용 API입니다.")
    public ResponseEntity<JobIdResponse> extractYoutubeRecipeV2(
            @RequestHeader("Idempotency-Key") String idempotencyKey,
            @RequestParam String url,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }

        Long userId = userDetails.getUser().getId();
        String nickname = userDetails.getUser().getNickname();

        Long jobId = recipeExtractionService.createYoutubeExtractionJobV2(url, userId, nickname, idempotencyKey);
        recipeExtractionService.processYoutubeExtractionAsyncV2(jobId, url, userId, nickname);

        return ResponseEntity.ok(new JobIdResponse(jobId));
    }

    @GetMapping("/youtube/status/{jobId}")
    @Operation(summary = "[V2] 유튜브 추출 상태 조회")
    public ResponseEntity<JobStatusDto> getYoutubeJobStatus(@PathVariable Long jobId) {
        return ResponseEntity.ok(recipeExtractionService.getJobStatus(jobId));
    }

    @GetMapping("/youtube/check")
    @Operation(summary = "유튜브 레시피 존재 여부 확인", description = "파라미터로 URL을 받아 DB 존재 여부를 확인합니다.")
    public ResponseEntity<RecipeIdResponse> checkYoutubeRecipeExistence(
            @Parameter(description = "유튜브 영상 URL", required = true)
            @RequestParam("url") String url
    ) {
        Long recipeId = recipeExtractionService.checkUrlExistence(url);

        if (recipeId != null) {
            return ResponseEntity.ok(new RecipeIdResponse(recipeId));
        } else {
            return ResponseEntity.ok(null);
        }
    }

    @GetMapping("/youtube/recommend")
    public ResponseEntity<List<YtDlpService.YoutubeSearchDto>> getRecommendedRecipes() {
        List<YtDlpService.YoutubeSearchDto> recommendations = recipeExtractionService.getRecommendedRecipes();
        return ResponseEntity.ok(recommendations);
    }

    public record RecipeIdResponse(
            @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
            Long recipeId
    ) {}
}