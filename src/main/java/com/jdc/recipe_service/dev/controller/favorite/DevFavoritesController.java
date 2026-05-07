package com.jdc.recipe_service.dev.controller.favorite;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleDto;
import com.jdc.recipe_service.dev.service.favorite.DevFavoritesService;
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
import org.springframework.web.bind.annotation.RestController;

/**
 * Dev V3 즐겨찾기 API.
 *
 * 운영 {@code /api/me/favorites} 미러. 차이점:
 *  - dev 정책 (PUBLIC+LISTED+ACTIVE + 본인 RESTRICTED/PRIVATE 추가) → 다른 사람 RESTRICTED/PRIVATE은 차단
 *  - imageReady 통일 (운영 PENDING 노출과 다름)
 *  - 응답 DTO에 4 enum 추가
 */
@RestController
@RequestMapping("/api/dev/me/favorites")
@RequiredArgsConstructor
@Tag(name = "Dev V3 즐겨찾기 API",
        description = "RESTRICTED 활성화 후 다른 사람 RESTRICTED/PRIVATE이 즐겨찾기에서 차단되는지 검증")
public class DevFavoritesController {

    private final DevFavoritesService devFavoritesService;

    @GetMapping
    @Operation(
            summary = "Dev V3 내 즐겨찾기 목록",
            description = """
                    운영 `/api/me/favorites` 미러. dev V3 차이점:
                      - **접근 정책**: `viewableBy(viewerId)` — 본인 ACTIVE PRIVATE 글 + 다른 사람 PUBLIC(LISTED+UNLISTED 모두) 노출.
                        link-only(PUBLIC+UNLISTED) 글도 즐겨찾기에 저장돼 있으면 보여야 함. 다른 사람 PRIVATE/RESTRICTED는 차단.
                        (운영의 `isPrivate=false OR PENDING` 단일 필터를 4-enum 정책으로 대체)
                      - **imageReady 통일**: PENDING/FAILED 차단 (운영의 PENDING 노출 정책과 다름 — A2/A3 정합)
                      - **응답 필드**: V1 base + visibility / lifecycleStatus / source / isRemix
                      - **인증 필수**
                      - **favoriteByCurrentUser는 항상 true** (즐겨찾기 목록이라)
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공",
                    content = @Content(schema = @Schema(implementation = DevRecipeSimpleDto.class))),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content)
    })
    public ResponseEntity<Page<DevRecipeSimpleDto>> getMyFavorites(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(description = "정렬 기준: `createdAt` (default, 즐겨찾기 추가 시각 — 운영 시맨틱 정합) | " +
                    "`recipeCreatedAt` (레시피 자체 생성일) | `cookingTime`. 그 외 값은 `createdAt`(=favorite.createdAt)으로 fallback.")
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        Page<DevRecipeSimpleDto> page = devFavoritesService.getMyFavoritesDev(userId, pageable);
        return ResponseEntity.ok(page);
    }
}
