package com.jdc.recipe_service.dev.controller.interaction;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.interaction.DevRatingService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingRequestDto;
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
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

/**
 * Dev V3 평점 API.
 *
 * 운영 {@code /api/ratings/recipe/*} 미러. dev V3 차이점은 {@link DevRatingService} 분기 게이트:
 *  - **POST**: pure update(기존 rating + 새 comment 없음)는 gate 우회. 신규 rating OR 새 comment 추가는 게이트 적용.
 *  - **GET me**: 자기 데이터 read → 게이트 없음.
 *  - **DELETE**: cleanup right → 게이트 없음.
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/ratings/recipe")
@Tag(name = "Dev V3 평점 API",
        description = "신규 rating/새 comment 추가 시 권한 게이트 — 기존 rating 값 변경/삭제는 항상 허용")
public class DevRecipeRatingController {

    private final DevRatingService devRatingService;

    @PostMapping("/{id}")
    @Operation(summary = "Dev V3 레시피 평점 등록/수정",
            description = """
                    운영 `POST /api/ratings/recipe/{id}` 미러. dev V3 차이점:
                      - **pure update (기존 rating + 빈 comment)**: gate 우회, rating 값만 직접 갱신 (운영 service 미호출 → 알림 발생 자체 불가)
                      - **신규 rating OR 새 comment 추가**: PRIVATE/RESTRICTED non-owner → 403, non-ACTIVE → 404. 통과 시 운영 service 위임 (comment 알림 포함)
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "등록/수정 성공 — {message: string}"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "신규 rating/comment 시 PRIVATE/RESTRICTED non-owner 시도 (RECIPE_PRIVATE_ACCESS_DENIED)",
                    content = @Content),
            @ApiResponse(responseCode = "404", description = "신규 rating/comment 시 레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Map<String, Object>> rateRecipe(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId Long id,
            @RequestBody RecipeRatingRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        devRatingService.rateRecipe(userId, id, dto);
        return ResponseEntity.ok(Map.of("message", "평점 등록 완료"));
    }

    @GetMapping("/{id}/me")
    @Operation(summary = "Dev V3 내 평점 조회",
            description = """
                    운영 `GET /api/ratings/recipe/{id}/me` 미러. dev V3 차이점 없음 — 자기 데이터 read는 게이트 없음.
                    rating이 없으면 0.0 반환 (운영과 동일).
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 — {rating: number}"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content)
    })
    public ResponseEntity<Map<String, Object>> getMyRating(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        Double rating = devRatingService.getMyRating(userId, id);
        return ResponseEntity.ok(Map.of("rating", rating));
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "Dev V3 평점 삭제",
            description = """
                    운영 `DELETE /api/ratings/recipe/{id}` 미러. dev V3 차이점 없음 — cleanup right이라 게이트 없음.
                    rating 없으면 RATING_NOT_FOUND 404.
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "삭제 성공 — {message: string}"),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "rating 없음 (RATING_NOT_FOUND) 또는 레시피 없음 (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Map<String, Object>> deleteRating(
            @Parameter(description = "레시피 ID (HashID)") @DecodeId Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();

        devRatingService.deleteRating(userId, id);
        return ResponseEntity.ok(Map.of("message", "평점 삭제 완료"));
    }
}
