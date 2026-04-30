package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.recipe.DevRecipeWriteService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateWithImageRequest;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Dev V3 recipe write API.
 *
 * 운영 {@code POST /api/recipes}, {@code PUT /api/recipes/{id}}, {@code DELETE /api/recipes/{id}} 미러.
 * dev V3 strict 정책은 {@link DevRecipeWriteService}가 담당:
 *  - <b>create</b>: remix origin 가시성 게이트 (RESTRICTED 원본 remix 차단)
 *  - <b>update</b>: owner + ACTIVE only (admin HIDDEN/BANNED 우회 차단)
 *  - <b>delete</b>: owner cleanup right (lifecycle 무관)
 *
 * 응답 shape은 운영과 동일 (PresignedUrlResponse / String message) — frontend swap-in 비용 최소화.
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/recipes")
@Tag(name = "Dev V3 레시피 쓰기 API",
        description = "create/update/delete — owner 강제 + lifecycle 정책 (update는 ACTIVE only) + remix origin 게이트")
public class DevRecipeWriteController {

    private final DevRecipeWriteService devRecipeWriteService;

    @PostMapping
    @Operation(summary = "Dev V3 레시피 직접 등록 + 이미지 Presigned URL 발급",
            description = """
                    운영 `POST /api/recipes` 미러. dev V3 차이점:
                      - **remix origin 가시성 게이트**: 요청에 originRecipeId가 있으면 origin이 RESTRICTED non-owner / non-ACTIVE면 차단
                        (운영 validateRemixSource는 official user만 검사 — dev는 한 단계 더)
                      - 통과 시 운영 RecipeService.createRecipeAndGenerateUrls에 위임 (USER source)
                      - **인증 필수**
                      - 응답: PresignedUrlResponse(recipeId, uploads) — 이미지 업로드용 presigned URL 포함
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "201", description = "생성 성공 — recipeId + presigned upload URLs"),
            @ApiResponse(responseCode = "400", description = "필수값 누락 (USER_RECIPE_IMAGE_REQUIRED 등)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "remix origin이 PRIVATE/RESTRICTED non-owner (RECIPE_PRIVATE_ACCESS_DENIED) 또는 RECIPE_REMIX_NOT_ALLOWED", content = @Content),
            @ApiResponse(responseCode = "404", description = "remix origin 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content),
            @ApiResponse(responseCode = "409", description = "remix 중복 (RECIPE_REMIX_ALREADY_EXISTS)", content = @Content)
    })
    public ResponseEntity<PresignedUrlResponse> createRecipe(
            @Valid @RequestBody RecipeWithImageUploadRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        PresignedUrlResponse response = devRecipeWriteService.createRecipe(userId, request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{recipeId}")
    @Operation(summary = "Dev V3 레시피 수정",
            description = """
                    운영 `PUT /api/recipes/{recipeId}` 미러. dev V3 차이점:
                      - **owner only + ACTIVE only**: admin이 HIDDEN/BANNED한 레시피는 owner여도 수정 불가 (404로 응답 — "존재하지 않는 것처럼")
                      - RESTRICTED/PRIVATE는 owner 본인 선택이라 수정 허용
                      - 통과 시 운영 RecipeService.updateUserRecipe에 위임 (재료 수정 시 AI 분석 + OpenSearch 색인 afterCommit)
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "수정 성공 — recipeId + presigned upload URLs"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 레시피 아님 (RECIPE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<PresignedUrlResponse> updateRecipe(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @Valid @RequestBody RecipeUpdateWithImageRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        PresignedUrlResponse response = devRecipeWriteService.updateRecipe(userId, recipeId, request);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{recipeId}")
    @Operation(summary = "Dev V3 레시피 삭제",
            description = """
                    운영 `DELETE /api/recipes/{recipeId}` 미러. dev V3 차이점 없음 — owner cleanup right.
                    HIDDEN/BANNED 레시피여도 owner는 본인 데이터 삭제 가능 (cleanup right 일관).
                    운영 service가 ownership(`recipe.user.id == userId`) 체크 + 이미지 S3 삭제 + OpenSearch 제거 afterCommit.
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "삭제 성공"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 레시피 아님 (RECIPE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<String> deleteRecipe(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        devRecipeWriteService.deleteRecipe(userId, recipeId);
        return ResponseEntity.ok("레시피가 성공적으로 삭제되었습니다.");
    }
}
