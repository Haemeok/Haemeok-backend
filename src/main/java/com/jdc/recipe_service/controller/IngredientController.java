package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.ingredient.*;
import com.jdc.recipe_service.service.IngredientService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import jakarta.validation.Valid;

@RestController
@RequestMapping("/api/ingredients")
@RequiredArgsConstructor
public class IngredientController {

    private final IngredientService service;

    /** 1) 조회
     * 전체 조회 또는 카테고리별 조회
     * GET  /api/ingredients?category={category}&page=0&size=20
     */
    @GetMapping
    public ResponseEntity<Page<IngredientResponseDto>> getAll(
            @RequestParam(required = false) String category,
            @PageableDefault(size = 20, sort = "name") Pageable pageable) {

        Page<IngredientResponseDto> page = service.getAll(category, pageable);
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
