package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.fridge.*;
import com.jdc.recipe_service.domain.type.IngredientType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RefrigeratorItemService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.*;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import jakarta.validation.Valid;

import java.util.Collections;

@RestController
@RequestMapping("/api/me/fridge")
@RequiredArgsConstructor
public class RefrigeratorItemController {

    private final RefrigeratorItemService service;

    /**
     * 1) 내 냉장고 아이템 조회
     * GET /api/me/fridge/items?category={category}&page=0&size=20&sort=createdAt,desc
     */
    @GetMapping("/items")
    public ResponseEntity<Page<RefrigeratorItemSummaryDto>> getMyItems(
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

    /** 2) 냉장고에 재료 추가 */
    @PostMapping("/items")
    public ResponseEntity<RefrigeratorItemResponseDto> addItem(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody @Valid RefrigeratorItemRequestDto dto) {
        Long userId = userDetails.getUser().getId();
        var created = service.addItem(userId, dto);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    /** 3) 냉장고에서 재료 제거 */
    @DeleteMapping("/items/{ingredientId}")
    public ResponseEntity<?> removeItem(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PathVariable Long ingredientId) {
        Long userId = userDetails.getUser().getId();
        service.removeItem(userId, ingredientId);
        return ResponseEntity.ok(Collections.emptyMap());
    }
    /** 4) 냉장고에 재료 여러 개 Bulk 추가 */
    @PostMapping("/items/bulk")
    public ResponseEntity<Void> addItemsBulk(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody @Valid RefrigeratorItemBulkRequestDto dto) {

        Long userId = userDetails.getUser().getId();
        service.addItemsBulk(userId, dto.getIngredientIds());
        return ResponseEntity.status(HttpStatus.CREATED).build();
    }

    /** 5) 냉장고에서 재료 여러 개 Bulk 제거 */
    @DeleteMapping("/items/bulk")
    public ResponseEntity<?> removeItemsBulk(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody @Valid RefrigeratorItemBulkRequestDto dto) {

        Long userId = userDetails.getUser().getId();
        service.removeItemsBulk(userId, dto.getIngredientIds());
        return ResponseEntity.ok(Collections.emptyMap());
    }

}
