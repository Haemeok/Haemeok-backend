package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.fridge.*;
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

import java.util.List;

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
    public ResponseEntity<Page<RefrigeratorItemResponseDto>> getMyItems(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestParam(required = false) String category,
            @PageableDefault(size = 20, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable) {

        Long userId = userDetails.getUser().getId();
        Page<RefrigeratorItemResponseDto> page =
                service.getMyItems(userId, category, pageable);

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
    public ResponseEntity<Void> removeItem(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PathVariable Long ingredientId) {
        Long userId = userDetails.getUser().getId();
        service.removeItem(userId, ingredientId);
        return ResponseEntity.noContent().build();
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
    public ResponseEntity<Void> removeItemsBulk(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestBody @Valid RefrigeratorItemBulkRequestDto dto) {

        Long userId = userDetails.getUser().getId();
        service.removeItemsBulk(userId, dto.getIngredientIds());
        return ResponseEntity.noContent().build();
    }

}
