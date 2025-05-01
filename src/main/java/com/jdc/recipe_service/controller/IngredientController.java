package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.ingredient.*;
import com.jdc.recipe_service.domain.type.IngredientType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.IngredientService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import jakarta.validation.Valid;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.http.HttpStatus;

@RestController
@RequestMapping("/api/ingredients")
@RequiredArgsConstructor
public class IngredientController {

    private final IngredientService service;

    /** 1) 검색 & 전체 조회 (모두 허용)
     * 전체 조회 또는 카테고리별 조회
     */
    @GetMapping
    public ResponseEntity<Page<IngredientSummaryDto>> search(
            @RequestParam(required = false, name = "category") String categoryCode,
            @RequestParam(required = false) String q,
            @RequestParam(required = false) String category,
            @RequestParam(required = false) Boolean inFridge,
            @PageableDefault(size = 20, sort = "name") Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;

        String koCategory = null;
        if (categoryCode != null && !categoryCode.isBlank()) {
            try {
                koCategory = IngredientType.fromCode(categoryCode).getKor();
            } catch (IllegalArgumentException e) {
                throw new CustomException(ErrorCode.INVALID_INGREDIENT_REQUEST);
            }
        }

        Page<IngredientSummaryDto> page =
                service.search(q, koCategory, inFridge, userId, pageable);
        return ResponseEntity.ok(page);
    }

    /** 3) 생성 - 관리자만 */
    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<IngredientResponseDto> create(
            @RequestBody @Valid IngredientRequestDto dto) {
        IngredientResponseDto created = service.create(dto);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    /** 4) 수정 - 관리자만 */
    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<IngredientResponseDto> update(
            @PathVariable Long id,
            @RequestBody @Valid IngredientRequestDto dto) {
        return ResponseEntity.ok(service.update(id, dto));
    }

    /** 5) 삭제 - 관리자만 */
    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<Void> delete(@PathVariable Long id) {
        service.delete(id);
        return ResponseEntity.noContent().build();
    }
}
