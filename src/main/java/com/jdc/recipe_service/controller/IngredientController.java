package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.ingredient.*;
import com.jdc.recipe_service.domain.type.IngredientType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.IngredientService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.ExampleObject;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.*;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.*;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/ingredients")
@RequiredArgsConstructor
@Tag(name = "재료 API", description = "재료 검색, 등록, 수정, 삭제를 위한 API입니다.")
public class IngredientController {

    private final IngredientService service;

    /** 1) 검색 & 전체 조회 (모두 허용)
     * 전체 조회 또는 카테고리별 조회
     */
    @GetMapping
    @Operation(summary = "재료 목록 조회", description = "전체 재료 또는 검색어, 카테고리, 냉장고 보유 여부로 재료를 필터링하여 조회합니다.")
    public ResponseEntity<Page<IngredientSummaryDto>> search(
            @Parameter(description = "재료 카테고리 코드", example = "VEGETABLE") @RequestParam(required = false, name = "category") String categoryCode,
            @Parameter(description = "검색어") @RequestParam(required = false) String q,
            @Parameter(hidden = true) @RequestParam(required = false) String category,
            @Parameter(description = "사용자의 냉장고에 있는 재료만 조회할지 여부") @RequestParam(required = false) Boolean inFridge,
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

        Page<IngredientSummaryDto> page = service.search(q, koCategory, userId, true, pageable);
        return ResponseEntity.ok(page);
    }

    /** 3) 생성 - 관리자만 */
    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "재료 등록", description = "새로운 재료를 등록합니다. (관리자만 가능)")
    public ResponseEntity<IngredientResponseDto> create(
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "생성할 재료 정보")
            @RequestBody @Valid IngredientRequestDto dto) {
        IngredientResponseDto created = service.create(dto);
        return ResponseEntity.status(HttpStatus.CREATED).body(created);
    }

    /** 4) 수정 - 관리자만 */
    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "재료 수정", description = "기존 재료 정보를 수정합니다. (관리자만 가능)")
    public ResponseEntity<IngredientResponseDto> update(
            @Parameter(description = "수정할 재료의 ID") @DecodeId Long id,
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "수정할 재료 정보")
            @RequestBody @Valid IngredientRequestDto dto) {
        return ResponseEntity.ok(service.update(id, dto));
    }

    /** 5) 삭제 - 관리자만 */
    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "재료 삭제", description = "지정한 재료를 삭제합니다. (관리자만 가능)")
    public ResponseEntity<Void> delete(
            @Parameter(description = "삭제할 재료의 ID") @DecodeId Long id) {
        service.delete(id);
        return ResponseEntity.noContent().build();
    }
    /** 재료 상세 조회 (i버튼 팝업용): 보관법 + Top 10 인기 레시피 */
    @GetMapping("/{id}")
    @Operation(
            summary = "재료 상세 조회",
            description = """
                    냉장고의 i버튼 팝업에서 사용할 재료 상세 정보를 반환합니다.
                    - 보관 방법(storageMethod)과 해당 재료로 만들 수 있는 **공개** 레시피를 인기순 최대 10개 포함합니다.
                    - 레시피 목록은 `isPrivate=false` 이고 이미지가 `READY` 또는 준비 중이 아닌 것만 포함합니다.
                    - 페이지네이션은 제공하지 않으며 `recipes`는 항상 배열(최대 10건, 빈 배열 가능)입니다.
                    - 인증이 필요 없는 공개 엔드포인트입니다.
                    """
    )
    @ApiResponses({
            @ApiResponse(
                    responseCode = "200",
                    description = "조회 성공",
                    content = @Content(schema = @Schema(implementation = IngredientDetailDto.class))
            ),
            @ApiResponse(
                    responseCode = "404",
                    description = "재료를 찾을 수 없음 (errorCode: INGREDIENT_NOT_FOUND)",
                    content = @Content(
                            examples = @ExampleObject(
                                    name = "INGREDIENT_NOT_FOUND",
                                    value = """
                                            {
                                              "code": "401",
                                              "message": "요청한 재료가 존재하지 않습니다.",
                                              "status": "NOT_FOUND"
                                            }
                                            """
                            )
                    )
            )
    })
    public ResponseEntity<IngredientDetailDto> getIngredientDetail(
            @Parameter(description = "재료 ID (해시)", example = "xJvY7aBp", required = true)
            @DecodeId("id") Long ingredientId) {
        return ResponseEntity.ok(service.findDetailById(ingredientId));
    }

    /** 6) 여러 ID에 대한 이름 조회 (해시 ID 리스트)*/
    @GetMapping("/names")
    @Operation(summary = "재료 이름 목록 조회", description = "해시화된 ID 리스트를 받아 해당 재료들의 이름 목록을 반환합니다.")
    public ResponseEntity<Map<String, List<IngredientIdNameDto>>> getIngredientNames(
            @Parameter(description = "해시화된 ID 리스트 (쉼표로 구분)") @RequestParam List<String> ids) {

        List<IngredientIdNameDto> content = service.findNamesByHashIds(ids);

        return ResponseEntity.ok(Map.of("content", content));
    }

}