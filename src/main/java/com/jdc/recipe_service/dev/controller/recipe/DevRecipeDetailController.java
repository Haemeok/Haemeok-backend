package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeDetailDto;
import com.jdc.recipe_service.dev.service.recipe.DevRecipeDetailService;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

/**
 * Dev V3 레시피 상세 API.
 *
 * V2 (`/api/v2/recipes/{id}`) 응답 shape를 그대로 유지하면서 dev 신규 필드 추가:
 *  - imageGenerationModel
 *  - visibility / lifecycleStatus / source
 *  - youtubeInfo (RecipeYoutubeInfo 분리 테이블 — legacy fallback)
 *  - extractionInfo (RecipeYoutubeExtractionInfo — evidence/cost)
 *  - ingredientCalculationSummary (1.2): raw 보존 + per-g 기반 새 계산 요약. base.totalCalories/
 *    totalIngredientCost는 V2 legacy 그대로 유지하고 이 필드가 별도 노출.
 *
 * 1.2 표시 정책:
 *  - 재료 라인의 name/quantity/unit이 raw_* > custom* > ingredient.* 우선순위로 노출됨
 *    (V2 legacy 표시 위에 dev에서 raw-first로 in-place 덮어씀).
 *
 * 인증: V2와 동일 — 비로그인도 공개 레시피 조회 가능 (서비스에서 visibility 검증).
 */
@Tag(name = "Dev Recipe Detail API", description = "레시피 상세 조회 (dev V3) — V2 + 신규 필드(youtube info, extraction info, model, visibility 등)")
@RestController
@RequestMapping("/api/dev/recipes")
@RequiredArgsConstructor
public class DevRecipeDetailController {

    private final DevRecipeDetailService devRecipeDetailService;

    @GetMapping("/{recipeId}")
    @Operation(summary = "레시피 상세 조회 (dev V3)",
            description = """
                    V2 응답 shape 위에 dev 추가 필드 노출.

                    추가 필드:
                    - youtubeInfo / extractionInfo / imageGenerationModel / visibility(+lifecycle/listing/source)
                    - ingredientCalculationSummary: raw 보존 + per-g 기반 새 계산 요약 (1.2). base.totalCalories와
                      base.totalIngredientCost는 V2 legacy 계산 그대로 유지하고, 이 필드가 새 정책 결과 + 4-state
                      카운트(MAPPED/PARTIAL/UNRESOLVED/CUSTOM) + calculated/pending 분리를 별도로 노출한다.

                    표시 변경:
                    - 재료 라인의 name/quantity/unit이 raw_* > custom* > ingredient.* 우선순위로 표시됨.
                      raw_*가 비어 있는 legacy row는 V2와 동일하게 customName/customUnit/legacy 컬럼으로 fallback.
                    """)
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "조회 성공",
                    content = @Content(schema = @Schema(implementation = DevRecipeDetailDto.class))),
            @ApiResponse(responseCode = "403",
                    description = "비공개 레시피 권한 없음 (errorCode=210 RECIPE_PRIVATE_ACCESS_DENIED)"),
            @ApiResponse(responseCode = "404",
                    description = "레시피 없음 (errorCode=201 RECIPE_NOT_FOUND)")
    })
    public ResponseEntity<DevRecipeDetailDto> getRecipeDetail(
            @DecodeId("recipeId") Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long currentUserId = userDetails != null ? userDetails.getUser().getId() : null;
        DevRecipeDetailDto dto = devRecipeDetailService.getRecipeDetail(recipeId, currentUserId);
        return ResponseEntity.ok(dto);
    }
}
