package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.recipebook.*;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeBookService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequiredArgsConstructor
@RequestMapping("/api/me/recipe-books")
@Tag(name = "레시피북 API", description = "레시피북(폴더) 생성, 조회, 수정, 삭제 및 레시피 관리 API입니다.")
public class RecipeBookController {

    private final RecipeBookService service;

    @PostMapping
    @Operation(summary = "레시피북 생성", description = "새 레시피북(폴더)을 생성합니다.")
    public ResponseEntity<RecipeBookResponse> createBook(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Valid @RequestBody CreateRecipeBookRequest request) {
        Long userId = extractUserId(userDetails);
        return ResponseEntity.status(HttpStatus.CREATED).body(service.createBook(userId, request));
    }

    @GetMapping
    @Operation(summary = "레시피북 목록 조회", description = "내 레시피북 목록을 표시 순서대로 조회합니다.")
    public ResponseEntity<List<RecipeBookResponse>> listBooks(
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = extractUserId(userDetails);
        return ResponseEntity.ok(service.listBooks(userId));
    }

    @GetMapping("/{bookId}")
    @Operation(summary = "레시피북 상세 조회",
            description = "레시피북의 상세 정보와 포함된 레시피 목록을 조회합니다.")
    public ResponseEntity<RecipeBookDetailResponse> getBookDetail(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @DecodeId Long bookId,
            @PageableDefault(size = 20, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable) {
        Long userId = extractUserId(userDetails);
        return ResponseEntity.ok(service.getBookDetail(userId, bookId, pageable));
    }

    @PatchMapping("/{bookId}")
    @Operation(summary = "레시피북 이름 변경", description = "레시피북의 이름을 변경합니다. 기본 레시피북은 변경할 수 없습니다.")
    public ResponseEntity<RecipeBookResponse> renameBook(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @DecodeId Long bookId,
            @Valid @RequestBody RenameRecipeBookRequest request) {
        Long userId = extractUserId(userDetails);
        return ResponseEntity.ok(service.renameBook(userId, bookId, request));
    }

    @DeleteMapping("/{bookId}")
    @Operation(summary = "레시피북 삭제", description = "레시피북을 삭제합니다. 기본 레시피북은 삭제할 수 없습니다.")
    public ResponseEntity<Map<String, String>> deleteBook(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @DecodeId Long bookId) {
        Long userId = extractUserId(userDetails);
        service.deleteBook(userId, bookId);
        return ResponseEntity.ok(Map.of("message", "레시피북이 삭제되었습니다."));
    }

    @PutMapping("/order")
    @Operation(summary = "레시피북 순서 변경", description = "레시피북의 표시 순서를 변경합니다.")
    public ResponseEntity<List<RecipeBookResponse>> reorderBooks(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Valid @RequestBody ReorderRecipeBooksRequest request) {
        Long userId = extractUserId(userDetails);
        return ResponseEntity.ok(service.reorderBooks(userId, request));
    }

    @PostMapping("/{bookId}/recipes")
    @Operation(summary = "레시피북에 레시피 추가 (bulk)",
            description = "레시피북에 레시피를 추가합니다. 이미 존재하거나 접근 불가한 레시피는 건너뜁니다.")
    public ResponseEntity<AddRecipesToBookResponse> addRecipes(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @DecodeId Long bookId,
            @Valid @RequestBody AddRecipesToBookRequest request) {
        Long userId = extractUserId(userDetails);
        AddRecipesToBookResponse result = service.addRecipesToBook(userId, bookId, request);
        return ResponseEntity.ok(result);
    }

    @DeleteMapping("/{bookId}/recipes")
    @Operation(summary = "레시피북에서 레시피 삭제",
            description = "레시피북에서 선택한 레시피들을 삭제합니다. (bulk)")
    public ResponseEntity<Map<String, String>> removeRecipes(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @DecodeId Long bookId,
            @Valid @RequestBody RemoveRecipesFromBookRequest request) {
        Long userId = extractUserId(userDetails);
        service.removeRecipesFromBook(userId, bookId, request);
        return ResponseEntity.ok(Map.of("message", "레시피가 레시피북에서 삭제되었습니다."));
    }

    private Long extractUserId(CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        return userDetails.getUser().getId();
    }
}
