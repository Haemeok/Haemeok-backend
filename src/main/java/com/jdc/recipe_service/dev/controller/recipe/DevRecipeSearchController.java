package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDto;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDtoV2;
import com.jdc.recipe_service.dev.service.search.DevRecipeSearchService;
import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
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
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

/**
 * Dev V3 search controller.
 *
 * 운영 V2 {@link com.jdc.recipe_service.controller.RecipeSearchControllerV2#search}와 동일한 query parameter
 * shape을 사용해서 프론트가 같은 hook으로 호출 가능하게 한다 (path만 dev로 분리).
 *
 * 응답: {@link DevRecipeSimpleStaticDto} (V2 base + 4 enum 추가).
 */
@RestController
@RequestMapping("/api/dev/recipes")
@RequiredArgsConstructor
@Tag(name = "Dev V3 레시피 검색 API", description = "RESTRICTED 누수 차단 + 4-enum 응답을 검증하기 위한 dev 검색 API")
public class DevRecipeSearchController {

    private final DevRecipeSearchService devRecipeSearchService;

    @GetMapping("/search")
    @Operation(
            summary = "Dev V3 조건 검색",
            description = """
                    운영 V2 검색과 동일한 query 인터페이스. dev V3 차이점:
                      - **접근 정책**: `lifecycle=ACTIVE && (owner OR (PUBLIC && LISTED))` 적용 →
                        RESTRICTED / PRIVATE / non-ACTIVE 누수 차단 (운영 V2의 `isPrivate=false` 단일 필터를 대체)
                      - **응답 필드**: V2 base + `visibility` / `listingStatus` / `lifecycleStatus` / `source` 4 enum 추가
                      - **Engine 분기**: `search.engine` (auto/opensearch/querydsl) + `search.dev-index.enabled` 조건
                        AND. dev mirror disabled면 OpenSearch 안 써서 빈 결과 사고 방지.
                      - **Pagination**: `page` (0-based), `size` (default 10), `sort` (createdAt | cookingTime, default createdAt,desc)
                      - **인증**: 선택 (anonymous면 PUBLIC+LISTED+ACTIVE만, 로그인 사용자면 자기 PRIVATE/RESTRICTED 추가)
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "검색 성공 (Page<DevRecipeSimpleStaticDto>)",
                    content = @Content(mediaType = "application/json",
                            schema = @Schema(implementation = DevRecipeSimpleStaticDto.class))),
            @ApiResponse(responseCode = "400", description = "잘못된 query parameter (잘못된 enum 값, sort 필드 등)",
                    content = @Content),
            @ApiResponse(responseCode = "500", description = "검색 인프라 장애 (OpenSearch + QueryDSL fallback 모두 실패 — 매우 드묾)",
                    content = @Content)
    })
    public ResponseEntity<Page<DevRecipeSimpleStaticDto>> search(
            @Parameter(description = "검색 조건 (제목, 디시타입, 태그, 영양성분, 가격 등 — 운영 V2와 동일)")
            @ModelAttribute RecipeSearchCondition cond,
            @Parameter(description = "페이지네이션 + 정렬 (createdAt | cookingTime). 동적 필드 정렬은 후속 phase.")
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long viewerId = (userDetails != null) ? userDetails.getUser().getId() : null;
        Page<DevRecipeSimpleStaticDto> page = devRecipeSearchService.searchRecipes(cond, pageable, viewerId);
        return ResponseEntity.ok(page);
    }

    @GetMapping("/popular")
    @Operation(
            summary = "Dev V3 인기 레시피",
            description = """
                    운영 V2 popular와 동일한 query 인터페이스. dev V3 차이점:
                      - **접근 정책**: lifecycle=ACTIVE && visibility=PUBLIC && listing=LISTED → RESTRICTED/PRIVATE/non-ACTIVE 누수 차단
                      - **응답 필드**: V2 base + 4 enum (visibility/listingStatus/lifecycleStatus/source)
                      - **Period**: `weekly` (default, weeklyLikeCount column 기반 fast path) / `monthly` (최근 30일 RecipeLike 실시간 COUNT) / 그 외 (2000-01-01 이후 모든 RecipeLike COUNT — 사실상 `all`)
                      - **Period fallback**: 알 수 없는 값(예: `quarterly`, 빈 문자열 외)은 `all` 시맨틱으로 처리 (운영 V2 parity, strict whitelist 없음)
                      - **Cache**: 운영 V2의 @Cacheable 미적용 — dev에서 fresh 응답 우선
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 (Page<DevRecipeSimpleStaticDto>)",
                    content = @Content(mediaType = "application/json",
                            schema = @Schema(implementation = DevRecipeSimpleStaticDto.class)))
    })
    public ResponseEntity<Page<DevRecipeSimpleStaticDto>> popular(
            @Parameter(description = "기간 (weekly | monthly | all)") @RequestParam(defaultValue = "weekly") String period,
            @PageableDefault(size = 10) Pageable pageable
    ) {
        Page<DevRecipeSimpleStaticDto> page = devRecipeSearchService.getPopularRecipes(period, pageable);
        return ResponseEntity.ok(page);
    }

    @GetMapping("/budget")
    @Operation(
            summary = "Dev V3 예산 레시피",
            description = """
                    운영 V2 budget과 동일한 query 인터페이스. dev V3 차이점:
                      - **접근 정책**: lifecycle=ACTIVE && visibility=PUBLIC && listing=LISTED + isAiGenerated=false
                      - **응답 필드**: V2 base + cost/marketPrice + 4 enum
                      - **로직**: top 10 인기 dev recipe 제외 + cost 범위 [1000, maxCost] + (favoriteCount * 0.3 + weeklyFavoriteCount * 0.7) 가중 정렬
                      - **maxCost fallback**: null 또는 음수면 Integer.MAX_VALUE로 처리 (운영 V2 parity, strict validation 없음)
                      - **Cache**: 운영 V2의 @Cacheable 미적용
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 (Page<DevRecipeSimpleStaticDtoV2>)",
                    content = @Content(mediaType = "application/json",
                            schema = @Schema(implementation = DevRecipeSimpleStaticDtoV2.class)))
    })
    public ResponseEntity<Page<DevRecipeSimpleStaticDtoV2>> budget(
            @Parameter(description = "최대 원가 (원)") @RequestParam(defaultValue = "10000") Integer maxCost,
            @PageableDefault(size = 10) Pageable pageable
    ) {
        Page<DevRecipeSimpleStaticDtoV2> page = devRecipeSearchService.getBudgetRecipes(maxCost, pageable);
        return ResponseEntity.ok(page);
    }
}
