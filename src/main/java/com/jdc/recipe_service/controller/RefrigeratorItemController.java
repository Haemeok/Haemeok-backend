package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.fridge.*;
import com.jdc.recipe_service.domain.type.IngredientType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RefrigeratorItemService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.*;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;

@RestController
@RequestMapping("/api/me/fridge")
@RequiredArgsConstructor
@Tag(name = "냉장고 API", description = "내 냉장고에 재료를 추가하거나 삭제하고, 현재 보유 중인 재료를 조회하는 기능을 제공합니다.")
public class RefrigeratorItemController {

    private final RefrigeratorItemService service;

    @GetMapping("/items")
    @Operation(summary = "내 냉장고 재료 조회", description = "카테고리별 또는 전체 냉장고 재료를 페이징하여 조회합니다.")
    public ResponseEntity<Page<RefrigeratorItemSummaryDto>> getMyItems(
            @Parameter(hidden = true)
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestParam(required = false, name = "category") String categoryCode,
            @PageableDefault(size = 20, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable) {

        Long userId = userDetails.getUser().getId();

        String koCategory = null;
        if (categoryCode != null && !categoryCode.isBlank()) {
            try {
                koCategory = IngredientType.fromCode(categoryCode).getKor();
            } catch (IllegalArgumentException e) {
                throw new CustomException(ErrorCode.INVALID_FRIDGE_REQUEST);
            }
        }

        Page<RefrigeratorItemSummaryDto> page =
                service.getMyItems(userId, koCategory, pageable);

        return ResponseEntity.ok(page);
    }

    @PostMapping("/items")
    @Operation(summary = "냉장고에 재료 추가", description = "내 냉장고에 새로운 재료를 추가합니다.")
    public ResponseEntity<RefrigeratorItemResponseDto> addItem(
            @Parameter(hidden = true)
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody @Valid RefrigeratorItemRequestDto dto) {
        Long userId = userDetails.getUser().getId();
        var created = service.addItem(userId, dto);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    @DeleteMapping("/items/{ingredientId}")
    @Operation(summary = "냉장고에서 재료 제거", description = "냉장고에서 특정 재료를 제거합니다.")
    public ResponseEntity<?> removeItem(
            @Parameter(hidden = true)
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PathVariable Long ingredientId) {
        Long userId = userDetails.getUser().getId();
        service.removeItem(userId, ingredientId);
        return ResponseEntity.ok(Collections.emptyMap());
    }

    @PostMapping("/items/bulk")
    @Operation(summary = "냉장고에 재료 여러 개 추가", description = "냉장고에 재료를 여러 개 한 번에 추가합니다.")
    public ResponseEntity<Void> addItemsBulk(
            @Parameter(hidden = true)
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody @Valid RefrigeratorItemBulkRequestDto dto) {

        Long userId = userDetails.getUser().getId();
        service.addItemsBulk(userId, dto.getIngredientIds());
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    @DeleteMapping("/items/bulk")
    @Operation(summary = "냉장고에서 재료 여러 개 제거", description = "냉장고에서 재료를 여러 개 한 번에 제거합니다.")
    public ResponseEntity<?> removeItemsBulk(
            @Parameter(hidden = true)
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody @Valid RefrigeratorItemBulkRequestDto dto) {

        Long userId = userDetails.getUser().getId();
        service.removeItemsBulk(userId, dto.getIngredientIds());
        return ResponseEntity.ok(Collections.emptyMap());
    }

}
