package com.jdc.recipe_service.dev.controller.recipebook;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.domain.dto.recipebook.DevRecipeBookDetailResponse;
import com.jdc.recipe_service.dev.service.recipebook.DevRecipeBookService;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeBookResponse;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Dev V3 레시피북 read API (목록 + 상세).
 *
 * <p>운영 {@code /api/me/recipe-books}, {@code /api/me/recipe-books/{bookId}} 미러. 차이점:
 * <ul>
 *   <li>book 안 레시피는 dev 정책({@code viewableBy(viewerId)} + imageReady) 적용 — 본인 ACTIVE PRIVATE + 다른 사람 PUBLIC(UNLISTED 포함) 노출, 다른 사람 RESTRICTED/PRIVATE 차단</li>
 *   <li>recipeCount는 dev 정책 통과 레시피만 (운영보다 엄격)</li>
 *   <li>book 상세 응답은 4 enum 노출 ({@link DevRecipeBookDetailResponse})</li>
 *   <li>book 자체(목록 항목)는 운영 {@link RecipeBookResponse} 그대로 — 정책 무관</li>
 * </ul>
 *
 * <p>Write 계열(create/rename/delete/addRecipes/removeRecipes/reorder)은 후속 phase.
 */
@RestController
@RequestMapping("/api/dev/me/recipe-books")
@RequiredArgsConstructor
@Tag(name = "Dev V3 레시피북 API",
        description = "RESTRICTED 활성화 후 다른 사람 RESTRICTED/PRIVATE이 레시피북에서 차단되는지 검증 (read-only, write는 후속)")
public class DevRecipeBookController {

    private final DevRecipeBookService devRecipeBookService;

    @GetMapping
    @Operation(
            summary = "Dev V3 레시피북 목록",
            description = """
                    운영 `/api/me/recipe-books` 미러. dev V3 차이점:
                      - **recipeCount는 dev 정책(`viewableBy`) 통과만**: 본인 ACTIVE PRIVATE + 다른 사람 PUBLIC(LISTED+UNLISTED 모두) 카운트.
                        link-only(PUBLIC+UNLISTED)도 폴더에 저장돼 있으면 카운트에 포함.
                      - 다른 사람의 PRIVATE/RESTRICTED/non-ACTIVE 레시피가 폴더에 들어 있어도 카운트 제외 (운영보다 엄격)
                      - book 자체 정보 (name/isDefault/displayOrder)는 운영과 동일
                      - **인증 필수**
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공 (List<RecipeBookResponse>)",
                    content = @Content(array = @ArraySchema(schema = @Schema(implementation = RecipeBookResponse.class)))),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content)
    })
    public ResponseEntity<List<RecipeBookResponse>> listBooks(
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        return ResponseEntity.ok(devRecipeBookService.listBooksDev(userId));
    }

    @GetMapping("/{bookId}")
    @Operation(
            summary = "Dev V3 레시피북 상세",
            description = """
                    운영 `/api/me/recipe-books/{bookId}` 미러. dev V3 차이점:
                      - **레시피 목록 정책**: `viewableBy(viewerId)` — 본인 ACTIVE PRIVATE + 다른 사람 PUBLIC(LISTED+UNLISTED 모두) 노출.
                        link-only(PUBLIC+UNLISTED) 글도 폴더에 저장돼 있으면 보여야 함. 다른 사람 PRIVATE/RESTRICTED는 차단.
                      - **imageReady**: PENDING/FAILED 차단 (A2/A3/User Lists 정합)
                      - **recipeCount**: dev 정책 통과 레시피만
                      - **응답 enum 노출**: DevRecipeBookItemResponse에 visibility / lifecycleStatus / source / isRemix
                      - **인증 필수**, ownership check (본인 폴더만)
                      - 정렬: `createdAt` (default, item.createdAt = 폴더 추가 시각) | `recipeCreatedAt` | `cookingTime`. fallback은 `createdAt`.
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공",
                    content = @Content(schema = @Schema(implementation = DevRecipeBookDetailResponse.class))),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 폴더 아님 (RECIPE_BOOK_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "폴더 없음 (RECIPE_BOOK_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<DevRecipeBookDetailResponse> getBookDetail(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(description = "레시피북 ID (HashID)", required = true) @DecodeId("bookId") Long bookId,
            @Parameter(description = "정렬 기준: `createdAt` (default, 폴더 추가 시각) | `recipeCreatedAt` | `cookingTime`. " +
                    "기본 size 20 — 운영 RecipeBookController parity.")
            @PageableDefault(size = 20, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        return ResponseEntity.ok(devRecipeBookService.getBookDetailDev(userId, bookId, pageable));
    }
}
