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
                    - 보관 정보(storageLocation/storageTemperature/storageDuration/storageNotes), 페어링(goodPairs/badPairs), 추천 조리법(recommendedCookingMethods), 그리고 해당 재료로 만들 수 있는 **공개** 레시피를 인기순 최대 10개 포함합니다.
                    - nutritionPer100g는 ingredients의 per-g 영양값을 100g 기준으로 환산한 값입니다.
                    - benefits/seasonMonths는 nullable 메타데이터입니다.
                    - goodPairItems/badPairItems는 goodPairs/badPairs를 파싱한 구조화 목록이며 DB에 존재하는 재료는 id/imageUrl을 포함합니다.
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

    @GetMapping("/{id}/units")
    @Operation(
            summary = "재료 단위 목록 조회",
            description = """
                    단일 재료에서 사용자가 선택할 수 있는 단위 목록을 반환합니다.
                    - units[].unit은 화면 표시 및 레시피 저장 시 ingredients[].unit으로 다시 전달하는 단위명입니다.
                    - 현재 저장 API는 ingredientUnitId를 받지 않고 서버가 재료명 + unit 문자열로 ingredient_units를 매칭합니다.
                    - gramsPerUnit은 사용자 표시용 총중량 기준 g입니다. 영양/원가 계산은 서버 내부에서 edibleGramsPerUnit으로 수행합니다.
                    - isDefault=true인 단위가 있으면 프론트 기본 선택값으로 사용할 수 있습니다.
                    """
    )
    @ApiResponses({
            @ApiResponse(
                    responseCode = "200",
                    description = "조회 성공",
                    content = @Content(schema = @Schema(implementation = IngredientUnitsResponse.class))
            ),
            @ApiResponse(responseCode = "404", description = "재료를 찾을 수 없음 (errorCode: INGREDIENT_NOT_FOUND)")
    })
    public ResponseEntity<IngredientUnitsResponse> getIngredientUnits(
            @Parameter(description = "재료 ID (HashID)", example = "xJvY7aBp", required = true)
            @DecodeId("id") Long ingredientId) {
        return ResponseEntity.ok(service.findUnitsByIngredientId(ingredientId));
    }

    @PostMapping("/units/batch")
    @Operation(
            summary = "여러 재료 단위 목록 일괄 조회",
            description = """
                    여러 재료 ID를 받아 각 재료에서 선택 가능한 단위 목록을 한 번에 반환합니다.
                    - 요청/응답의 ingredientId는 HashID입니다.
                    - units[].unit은 화면 표시 및 레시피 저장 시 ingredients[].unit으로 다시 전달하는 단위명입니다.
                    - 응답 items는 요청 ingredientIds의 중복 제거 후 순서를 유지합니다.
                    - 존재하지만 단위가 없는 재료는 units=[]로 반환될 수 있습니다.
                    """
    )
    @ApiResponses({
            @ApiResponse(
                    responseCode = "200",
                    description = "조회 성공",
                    content = @Content(schema = @Schema(implementation = IngredientUnitsBatchResponse.class))
            ),
            @ApiResponse(responseCode = "400", description = "ingredientIds가 비어 있거나 잘못됨 (errorCode: INVALID_INGREDIENT_REQUEST)"),
            @ApiResponse(responseCode = "404", description = "요청한 재료 중 존재하지 않는 ID가 있음 (errorCode: INGREDIENT_NOT_FOUND)")
    })
    public ResponseEntity<IngredientUnitsBatchResponse> getIngredientUnitsBatch(
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "재료 ID(HashID) 목록")
            @RequestBody @Valid IngredientUnitsBatchRequest request) {
        return ResponseEntity.ok(service.findUnitsByIngredientIds(request.getIngredientIds()));
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
