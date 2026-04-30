package com.jdc.recipe_service.dev.controller.recipebook;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.recipebook.DevRecipeBookWriteService;
import com.jdc.recipe_service.domain.dto.recipebook.AddRecipesToBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.AddRecipesToBookResponse;
import com.jdc.recipe_service.domain.dto.recipebook.CreateRecipeBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeBookResponse;
import com.jdc.recipe_service.domain.dto.recipebook.RemoveRecipesFromBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.RenameRecipeBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.ReorderRecipeBooksRequest;
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
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;
import java.util.Map;

/**
 * Dev V3 recipe-book write API.
 *
 * 운영 {@code /api/me/recipe-books/*} 미러. 6 endpoint:
 *  - create / rename / delete / reorder: 단순 위임 (ownership/default/duplicate는 운영이 검증)
 *  - addRecipes: **dev V3 strict** — RESTRICTED 추가 차단 (운영은 isPrivate만 검사)
 *  - removeRecipes: cleanup right — 게이트 없음
 */
@RestController
@RequiredArgsConstructor
@RequestMapping("/api/dev/me/recipe-books")
@Tag(name = "Dev V3 레시피북 쓰기 API",
        description = "create/rename/delete/reorder/add/remove. addRecipes만 dev V3 strict (RESTRICTED 차단), 나머지는 운영 위임")
public class DevRecipeBookWriteController {

    private final DevRecipeBookWriteService devRecipeBookWriteService;

    @PostMapping
    @Operation(summary = "Dev V3 레시피북 생성",
            description = """
                    운영 `POST /api/me/recipe-books` 미러. dev V3 차이점 없음 — 단순 위임 (ownership 무관, 본인 user 자동 적용).
                    - 최대 50개 제한, 이름 중복 검사 등은 운영이 처리
                    - 인증 필수
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "201", description = "생성 성공"),
            @ApiResponse(responseCode = "400", description = "한도 초과 (RECIPE_BOOK_LIMIT_EXCEEDED) 또는 payload 누락/검증 실패 (INVALID_INPUT_VALUE)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요 (UNAUTHORIZED)", content = @Content),
            @ApiResponse(responseCode = "409", description = "이름 중복 (RECIPE_BOOK_DUPLICATE_NAME)", content = @Content)
    })
    public ResponseEntity<RecipeBookResponse> createBook(
            @Valid @RequestBody CreateRecipeBookRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = extractUserId(userDetails);
        return ResponseEntity.status(HttpStatus.CREATED).body(devRecipeBookWriteService.createBook(userId, request));
    }

    @PatchMapping("/{bookId}")
    @Operation(summary = "Dev V3 레시피북 이름 변경",
            description = """
                    운영 `PATCH /api/me/recipe-books/{bookId}` 미러. 단순 위임.
                    - 기본 폴더 변경 불가 (RECIPE_BOOK_DEFAULT_CANNOT_RENAME)
                    - 이름 중복 검사
                    - 인증 + ownership 필수
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "이름 변경 성공"),
            @ApiResponse(responseCode = "400", description = "기본 폴더 (RECIPE_BOOK_DEFAULT_CANNOT_RENAME) 또는 payload 누락/검증 실패 (INVALID_INPUT_VALUE)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 폴더 아님 (RECIPE_BOOK_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "폴더 없음 (RECIPE_BOOK_NOT_FOUND)", content = @Content),
            @ApiResponse(responseCode = "409", description = "이름 중복 (RECIPE_BOOK_DUPLICATE_NAME)", content = @Content)
    })
    public ResponseEntity<RecipeBookResponse> renameBook(
            @Parameter(description = "레시피북 ID (HashID)") @DecodeId("bookId") Long bookId,
            @Valid @RequestBody RenameRecipeBookRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = extractUserId(userDetails);
        return ResponseEntity.ok(devRecipeBookWriteService.renameBook(userId, bookId, request));
    }

    @DeleteMapping("/{bookId}")
    @Operation(summary = "Dev V3 레시피북 삭제",
            description = """
                    운영 `DELETE /api/me/recipe-books/{bookId}` 미러. 단순 위임.
                    - 기본 폴더 삭제 불가 (RECIPE_BOOK_DEFAULT_CANNOT_DELETE)
                    - 이 폴더에만 있던 레시피는 legacy favorite도 함께 제거됨
                    - 인증 + ownership 필수
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "삭제 성공"),
            @ApiResponse(responseCode = "400", description = "기본 폴더 (RECIPE_BOOK_DEFAULT_CANNOT_DELETE)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 폴더 아님 (RECIPE_BOOK_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "폴더 없음 (RECIPE_BOOK_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Map<String, String>> deleteBook(
            @Parameter(description = "레시피북 ID (HashID)") @DecodeId("bookId") Long bookId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = extractUserId(userDetails);
        devRecipeBookWriteService.deleteBook(userId, bookId);
        return ResponseEntity.ok(Map.of("message", "레시피북이 삭제되었습니다."));
    }

    @PutMapping("/order")
    @Operation(summary = "Dev V3 레시피북 순서 변경",
            description = """
                    운영 `PUT /api/me/recipe-books/order` 미러. 단순 위임.
                    - bookIds는 본인 모든 폴더의 ID와 정확히 일치해야 함 (set match)
                    - 인증 필수
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "순서 변경 성공"),
            @ApiResponse(responseCode = "400", description = "ID 목록 불일치 (INVALID_INPUT_VALUE)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요", content = @Content)
    })
    public ResponseEntity<List<RecipeBookResponse>> reorderBooks(
            @Valid @RequestBody ReorderRecipeBooksRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = extractUserId(userDetails);
        return ResponseEntity.ok(devRecipeBookWriteService.reorderBooks(userId, request));
    }

    @PostMapping("/{bookId}/recipes")
    @Operation(summary = "Dev V3 레시피북에 레시피 추가 (bulk)",
            description = """
                    운영 `POST /api/me/recipe-books/{bookId}/recipes` 미러. dev V3 차이점:
                      - **🚨 운영 leak 차단**: 운영은 `isPrivate=false`만 검사 → RESTRICTED(=isPrivate=false) 통과 → 누구나 RESTRICTED 추가 가능.
                        dev V3는 batch projection으로 4-enum `accessibleBy` strict 필터 → RESTRICTED/non-ACTIVE/존재 안 함 모두 차단
                      - **skippedCount는 합산**: 운영 skipped (이미 폴더에 있음) + dev filtered (접근 불가) — 클라이언트는 합산값만 받음
                      - 통과 ID는 운영 service에 위임 (legacy favorite 동기화 포함)
                      - 인증 + book ownership 필수
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "추가 성공 — {addedCount, skippedCount}"),
            @ApiResponse(responseCode = "400", description = "payload 누락/검증 실패 — recipeIds 누락 또는 빈 리스트 (INVALID_INPUT_VALUE)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 폴더 아님 (RECIPE_BOOK_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "폴더 없음 (RECIPE_BOOK_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<AddRecipesToBookResponse> addRecipes(
            @Parameter(description = "레시피북 ID (HashID)") @DecodeId("bookId") Long bookId,
            @Valid @RequestBody AddRecipesToBookRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = extractUserId(userDetails);
        return ResponseEntity.ok(devRecipeBookWriteService.addRecipesToBook(userId, bookId, request));
    }

    @DeleteMapping("/{bookId}/recipes")
    @Operation(summary = "Dev V3 레시피북에서 레시피 삭제 (bulk)",
            description = """
                    운영 `DELETE /api/me/recipe-books/{bookId}/recipes` 미러. 단순 위임 — cleanup right (RESTRICTED 레시피라도 본인 폴더에서 제거 가능).
                    - 마지막 폴더에서 빠진 레시피는 legacy favorite도 제거됨
                    - 인증 + book ownership 필수
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "삭제 성공"),
            @ApiResponse(responseCode = "400", description = "payload 누락/검증 실패 — recipeIds 누락 또는 빈 리스트 (INVALID_INPUT_VALUE)", content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요", content = @Content),
            @ApiResponse(responseCode = "403", description = "본인 폴더 아님 (RECIPE_BOOK_ACCESS_DENIED)", content = @Content),
            @ApiResponse(responseCode = "404", description = "폴더 없음 (RECIPE_BOOK_NOT_FOUND)", content = @Content)
    })
    public ResponseEntity<Map<String, String>> removeRecipes(
            @Parameter(description = "레시피북 ID (HashID)") @DecodeId("bookId") Long bookId,
            @Valid @RequestBody RemoveRecipesFromBookRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = extractUserId(userDetails);
        devRecipeBookWriteService.removeRecipesFromBook(userId, bookId, request);
        return ResponseEntity.ok(Map.of("message", "레시피가 레시피북에서 삭제되었습니다."));
    }

    private Long extractUserId(CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        return userDetails.getUser().getId();
    }
}
