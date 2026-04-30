package com.jdc.recipe_service.dev.controller.ingredient;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.service.ingredient.DevIngredientService;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientDetailDto;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * Dev V3 ingredient API.
 *
 * 운영 {@code /api/ingredients/{id}}와 동일한 응답 shape({@link IngredientDetailDto})을 반환하되,
 * `recipes` 필드만 dev 정책(PUBLIC+LISTED+ACTIVE) 통과 레시피로 교체.
 *
 * 보관 정보/페어링/조리법/nutritionPer100g/benefits/seasonMonths/structured pair fields는 dev/운영 동일.
 */
@RestController
@RequestMapping("/api/dev/ingredients")
@RequiredArgsConstructor
@Tag(name = "Dev V3 재료 API", description = "RESTRICTED 누수 차단된 top recipe를 검증하기 위한 dev 재료 API")
public class DevIngredientController {

    private final DevIngredientService devIngredientService;

    @GetMapping("/{id}")
    @Operation(
            summary = "Dev V3 재료 상세 조회",
            description = """
                    운영 `/api/ingredients/{id}`와 동일한 응답 shape. 차이점:
                      - **recipes 필드 정책**: lifecycle=ACTIVE && visibility=PUBLIC && listing=LISTED 통과 레시피만 (운영의 `isPrivate=false` 대체)
                      - 보관 정보/페어링/조리법/nutritionPer100g/benefits/seasonMonths/structured pair fields는 운영과 동일
                      - 인증 불필요 (운영과 동일)
                    """
    )
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공",
                    content = @Content(schema = @Schema(implementation = IngredientDetailDto.class))),
            @ApiResponse(responseCode = "404", description = "재료를 찾을 수 없음 (errorCode: INGREDIENT_NOT_FOUND)",
                    content = @Content)
    })
    public ResponseEntity<IngredientDetailDto> getIngredientDetail(
            @Parameter(description = "재료 ID (해시)", required = true)
            @DecodeId("id") Long ingredientId) {
        return ResponseEntity.ok(devIngredientService.findDetailByIdDev(ingredientId));
    }
}
