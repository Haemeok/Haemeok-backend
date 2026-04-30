package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.recipe.DevRecipeRelatedService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Dev V3 레시피 관련 조회 API.
 *
 * 운영 {@code /api/recipes/{id}/recommendations}, {@code /api/recipes/{id}/remixes} 미러.
 * dev V3 차이점은 {@link DevRecipeRelatedService}가 담당 — base recipe 가시성 게이트 적용:
 *  - RESTRICTED/PRIVATE non-owner 또는 non-ACTIVE 레시피 → 게이트 차단 (anonymous probing 불가)
 *  - 통과 시 운영 service 결과 그대로 반환
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/recipes")
@Tag(name = "Dev V3 레시피 관련 조회 API",
        description = "추천/리믹스 — base 레시피 가시성 게이트로 RESTRICTED/PRIVATE/non-ACTIVE 존재 누설 차단")
public class DevRecipeRelatedController {

    private final DevRecipeRelatedService devRecipeRelatedService;

    @GetMapping("/{id}/recommendations")
    @Operation(summary = "Dev V3 상세 페이지 하단 추천 레시피",
            description = """
                    운영 `GET /api/recipes/{id}/recommendations` 미러. dev V3 차이점:
                      - **base recipe 가시성 게이트**: PRIVATE/RESTRICTED non-owner → 403, non-ACTIVE → 404
                      - 통과 시 운영 RecipeRecommendationService에 위임 (캐시 + dish-type pairing 알고리즘 그대로)
                      - **post-filter (이중 방어)**: 운영 후보 쿼리는 isPrivate=false / imageStatus 필터가 없음 → batch projection으로
                        가시성(accessibleBy) + imageReady(READY OR NULL) 둘 다 통과한 ID만 남김
                      - **결과 size가 요청 size 미만일 수 있음**: 추천 알고리즘이 내준 후보 중 일부가 RESTRICTED/non-ACTIVE/PENDING/FAILED로
                        post-filter에서 제외되면 응답 list가 짧아짐. 클라이언트는 정확한 N개를 보장받지 못함
                      - 인증 선택 — 비로그인은 PUBLIC+LISTED+ACTIVE base 레시피만 통과
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 — List<RecipeSimpleStaticDto>"),
            @ApiResponse(responseCode = "403", description = "PRIVATE/RESTRICTED 레시피 non-owner (RECIPE_PRIVATE_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "레시피 없음 또는 non-ACTIVE (RECIPE_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<List<RecipeSimpleStaticDto>> getRecommendations(
            @Parameter(description = "기준 레시피 ID (HashID)") @DecodeId("id") Long recipeId,
            @Parameter(description = "추천 개수 (default 10)") @RequestParam(defaultValue = "10") int size,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long viewerId = userDetails != null ? userDetails.getUser().getId() : null;
        return ResponseEntity.ok(devRecipeRelatedService.getRecommendations(viewerId, recipeId, size));
    }

    @GetMapping("/{recipeId}/remixes")
    @Operation(summary = "Dev V3 레시피 리믹스 목록",
            description = """
                    운영 `GET /api/recipes/{id}/remixes` 미러. dev V3 차이점:
                      - **base 원본 가시성 게이트**: PRIVATE/RESTRICTED non-owner → 403, non-ACTIVE → 404
                      - **dev 전용 strict repo 사용**: 운영의 4-enum 필터(PUBLIC+LISTED+ACTIVE)에 더해 imageReady(READY OR NULL) 추가 적용 →
                        PENDING/FAILED remix 카드도 차단 (운영 RecipeRemixQueryService는 미적용)
                      - count + list 동일 WHERE → totalElements와 list.size 일치 보장
                      - 정렬: default `popularityScore` DESC
                      - 인증 선택
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 — Page<RecipeSimpleDto>"),
            @ApiResponse(responseCode = "403", description = "PRIVATE/RESTRICTED 원본 non-owner", content = @Content),
            @ApiResponse(responseCode = "404", description = "원본 없음 또는 non-ACTIVE", content = @Content)
    })
    public ResponseEntity<Page<RecipeSimpleDto>> getRemixes(
            @Parameter(description = "원본 레시피 ID (HashID)") @DecodeId("recipeId") Long recipeId,
            @PageableDefault(size = 10, sort = "popularityScore", direction = Sort.Direction.DESC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long viewerId = userDetails != null ? userDetails.getUser().getId() : null;
        return ResponseEntity.ok(devRecipeRelatedService.findRemixes(viewerId, recipeId, pageable));
    }
}
