package com.jdc.recipe_service.dev.controller.user;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevMyRecipeSummaryDto;
import com.jdc.recipe_service.dev.service.user.DevUserRecipesService;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
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
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Dev V3 user/me recipes API.
 *
 * 두 endpoint 동일 service 호출 — viewer 시점 분기는 service의 {@link com.jdc.recipe_service.dev.repository.recipe.DevRecipeQueryPredicates}
 * 가 자동 처리:
 *  - GET /api/dev/users/{userId}/recipes: 타인 프로필. anonymous OK. 본인이 자기 호출하면 owner 분기 자동.
 *  - GET /api/dev/me/recipes: 내 목록. 인증 필수. service에 viewerId == targetUserId 전달 → owner 분기.
 *
 * RESTRICTED 활성화 후 검증의 핵심 경로 — "내 눈에는 보이고 남에게는 안 보이는지" 실제 UX 확인.
 */
@RestController
@RequiredArgsConstructor
@Tag(name = "Dev V3 사용자/내 레시피 API",
        description = "RESTRICTED 활성화 후 owner 분기 검증 — 타인 프로필은 ACTIVE+PUBLIC+LISTED, 본인은 ACTIVE PRIVATE/RESTRICTED 추가 노출")
public class DevUserRecipesController {

    private final DevUserRecipesService devUserRecipesService;

    @GetMapping("/api/dev/users/{userId}/recipes")
    @Operation(
            summary = "Dev V3 사용자 레시피 목록",
            description = """
                    운영 `/api/users/{userId}/recipes` 미러. 차이점:
                      - **접근 정책**: viewer 시점 `accessibleBy` (운영의 isPrivate=false 단일 필터 대체)
                        * 본인이 자기 프로필 보면 ACTIVE PRIVATE/RESTRICTED 모두 노출 (owner 분기)
                        * 타인이 보면 ACTIVE+PUBLIC+LISTED만 노출 (RESTRICTED 누수 차단)
                      - **lifecycle**: ACTIVE만 (HIDDEN/BANNED/DELETED는 owner도 차단 — admin 우회 방지)
                      - **응답 필드**: V1 base + 4 enum (visibility/listingStatus/lifecycleStatus/source)
                      - **인증**: 선택 (anonymous면 PUBLIC+LISTED+ACTIVE만)
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공",
                    content = @Content(schema = @Schema(implementation = DevMyRecipeSummaryDto.class))),
            @ApiResponse(responseCode = "400", description = "잘못된 query parameter (types에 잘못된 RecipeSourceType enum 값 등)",
                    content = @Content),
            @ApiResponse(responseCode = "404", description = "사용자 없음 (USER_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Page<DevMyRecipeSummaryDto>> getUserRecipes(
            @Parameter(description = "대상 사용자 ID (HashID)", required = true) @DecodeId Long userId,
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(description = "source 필터 (USER/AI/YOUTUBE/REELS, optional)")
            @RequestParam(required = false) List<RecipeSourceType> types,
            @Parameter(description = "정렬 기준: `createdAt` (default) | `cookingTime`. 그 외 값은 `createdAt`으로 fallback. " +
                    "동적 필드(likeCount/avgRating 등) 정렬은 후속 phase.")
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable) {

        Long viewerId = (userDetails != null) ? userDetails.getUser().getId() : null;
        Page<DevMyRecipeSummaryDto> page = devUserRecipesService.getUserRecipesDev(userId, viewerId, types, pageable);
        return ResponseEntity.ok(page);
    }

    @GetMapping("/api/dev/me/recipes")
    @Operation(
            summary = "Dev V3 내 레시피 목록",
            description = """
                    운영 `/api/me/recipes` 미러. dev V3 차이점:
                      - **viewer = owner**이므로 ACTIVE PRIVATE/RESTRICTED 모두 노출
                      - **lifecycle**: ACTIVE만 (HIDDEN/BANNED/DELETED는 owner도 차단 — 운영의 owner-all-visible과 다름)
                      - **응답 필드**: V1 base + 4 enum
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공",
                    content = @Content(schema = @Schema(implementation = DevMyRecipeSummaryDto.class))),
            @ApiResponse(responseCode = "400", description = "잘못된 query parameter (types에 잘못된 RecipeSourceType enum 값 등)",
                    content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content)
    })
    public ResponseEntity<Page<DevMyRecipeSummaryDto>> getMyRecipes(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(description = "source 필터 (USER/AI/YOUTUBE/REELS, optional)")
            @RequestParam(required = false) List<RecipeSourceType> types,
            @Parameter(description = "정렬 기준: `createdAt` (default) | `cookingTime`. 그 외 값은 `createdAt`으로 fallback.")
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long viewerId = userDetails.getUser().getId();
        // viewer == targetUserId → service의 accessibleBy가 자동으로 owner 분기로 떨어짐
        Page<DevMyRecipeSummaryDto> page = devUserRecipesService.getUserRecipesDev(viewerId, viewerId, types, pageable);
        return ResponseEntity.ok(page);
    }
}
