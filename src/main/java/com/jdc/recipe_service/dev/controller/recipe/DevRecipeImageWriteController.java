package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.recipe.DevRecipeImageWriteService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeImageKeyUpdateRequest;
import com.jdc.recipe_service.domain.dto.url.FinalizeResponse;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlResponse;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Dev V3 recipe 이미지 write API.
 *
 * 운영 {@code POST /api/recipes/{id}/presigned-urls}, {@code POST /api/recipes/{id}/finalize},
 * {@code PUT /api/recipes/{id}/images} 미러. dev V3 차이점은 {@link DevRecipeImageWriteService}가 담당:
 *  - 모든 endpoint: owner + ACTIVE only (운영 presigned-urls는 ownership 자체 안 봐서 critical leak)
 *  - finalize: admin escape 없음 (관리자는 운영 endpoint 사용)
 *  - updateImageKeys: payload null guard 추가
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/recipes")
@Tag(name = "Dev V3 레시피 이미지 쓰기 API",
        description = "presigned URL / finalize / image keys — owner + ACTIVE 게이트 + 운영 leak 차단")
public class DevRecipeImageWriteController {

    private final DevRecipeImageWriteService devRecipeImageWriteService;

    @PostMapping("/{recipeId}/presigned-urls")
    @Operation(summary = "Dev V3 레시피 이미지 Presigned URL 발급",
            description = """
                    운영 `POST /api/recipes/{recipeId}/presigned-urls` 미러. dev V3 차이점:
                      - **🚨 운영 leak 차단**: 운영 RecipeUploadService.generatePresignedUrlsForUpdate는 ownership 검사 자체가 없어
                        누구나 임의 recipeId로 다른 사람 이미지 폴더에 업로드 가능. dev V3가 owner + ACTIVE pre-check로 차단
                      - **request body 또는 files=null → 400 INVALID_INPUT_VALUE** (운영 service의 stream() NPE 회피, pre-DB 차단)
                      - 빈 files 리스트는 허용 (운영 동작과 동일 — 빈 uploads 응답)
                      - non-ACTIVE 레시피 (HIDDEN/BANNED) → 404 (admin 우회 차단, Batch 1 update와 일관)
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "발급 성공 — uploads (fileKey + presigned URL)"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 레시피 아님 (RECIPE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<UpdatePresignedUrlResponse> getPresignedUrlsForUpdate(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @RequestBody UpdatePresignedUrlRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        UpdatePresignedUrlResponse response = devRecipeImageWriteService.generatePresignedUrlsForUpdate(
                userId, recipeId, request != null ? request.getFiles() : null);
        return ResponseEntity.ok(response);
    }

    @PostMapping("/{recipeId}/finalize")
    @Operation(summary = "Dev V3 레시피 이미지 업로드 완료 처리",
            description = """
                    운영 `POST /api/recipes/{recipeId}/finalize` 미러. dev V3 차이점:
                      - owner + ACTIVE only — **admin escape 없음** (관리자는 운영 endpoint 사용)
                      - non-ACTIVE 레시피 → 404
                      - 통과 시 운영 RecipeService.finalizeRecipeImages 위임 (이미지 활성화 + OpenSearch 색인 afterCommit)
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "처리 성공 — FinalizeResponse(recipeId, activeImages, missingFiles)"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 레시피 아님 (RECIPE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<FinalizeResponse> finalizeRecipeImages(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        FinalizeResponse response = devRecipeImageWriteService.finalizeRecipeImages(userId, recipeId);
        return ResponseEntity.ok(response);
    }

    @PutMapping("/{recipeId}/images")
    @Operation(summary = "Dev V3 레시피 이미지 키 업데이트",
            description = """
                    운영 `PUT /api/recipes/{recipeId}/images` 미러. dev V3 차이점:
                      - owner + ACTIVE only (운영은 owner check 있음, dev는 ACTIVE 추가)
                      - **payload null guard**: request 자체가 null이면 400 INVALID_INPUT_VALUE
                      - **stepImageKeys=null normalize**: 빈 리스트로 변환하여 main-image-only update 지원
                        (운영은 IntStream.range(0, stepImageKeys.size()) NPE 발생 — dev에서 normalize)
                      - 통과 시 운영 RecipeService.updateImageKeys 위임 (USER 레시피는 imageKey 필수)
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "업데이트 성공 (응답 body 없음)"),
            @ApiResponse(responseCode = "400", description = "USER 레시피인데 imageKey 누락 (USER_RECIPE_IMAGE_REQUIRED) 또는 payload null (INVALID_INPUT_VALUE)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 레시피 아님 (RECIPE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Void> updateRecipeImageKeys(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @RequestBody RecipeImageKeyUpdateRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        devRecipeImageWriteService.updateImageKeys(userId, recipeId, request);
        return ResponseEntity.ok().build();
    }
}
