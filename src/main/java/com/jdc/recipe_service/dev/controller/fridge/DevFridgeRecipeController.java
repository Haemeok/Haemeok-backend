package com.jdc.recipe_service.dev.controller.fridge;

import com.jdc.recipe_service.dev.service.fridge.DevFridgeRecipeService;
import com.jdc.recipe_service.domain.dto.recipe.FridgeRecipeDto;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * Dev V3 fridge 추천 API.
 *
 * 운영 {@code /api/me/fridge/recipes}와 동일한 응답 shape({@link FridgeRecipeDto}). 차이점:
 *  - 추천 풀이 4-enum 정책(PUBLIC+LISTED+ACTIVE) 통과 레시피로 한정 → RESTRICTED 누수 차단
 *  - 인증 필요 (운영과 동일)
 */
@RestController
@RequestMapping("/api/dev/me/fridge/recipes")
@RequiredArgsConstructor
@Tag(name = "Dev V3 냉장고 추천 API", description = "RESTRICTED 누수 차단된 fridge 추천을 검증하기 위한 dev API")
public class DevFridgeRecipeController {

    private final DevFridgeRecipeService devFridgeRecipeService;

    @GetMapping
    @Operation(
            summary = "Dev V3 냉장고 기반 레시피 추천",
            description = """
                    운영 `/api/me/fridge/recipes`와 동일한 query 인터페이스 + 응답 shape. 차이점:
                      - **추천 풀**: lifecycle=ACTIVE && visibility=PUBLIC && listing=LISTED 통과 레시피만
                      - **types 기본값**: 운영과 동일 (`USER`, `YOUTUBE` — `null`/빈 list일 때)
                      - **인증 필요**: 운영과 동일 (userId 기반 fridge 조회)
                      - **응답 필드**: 운영과 동일 ({@code FridgeRecipeDto} — `RecipeSimpleDto` + `matchedIngredients` + `missingIngredients`)
                    """
    )
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "추천 성공",
                    content = @Content(schema = @Schema(implementation = FridgeRecipeDto.class))),
            @ApiResponse(responseCode = "400", description = "잘못된 query parameter (types에 잘못된 RecipeType enum 값 등)",
                    content = @Content),
            @ApiResponse(responseCode = "401", description = "인증 필요", content = @Content)
    })
    public ResponseEntity<Slice<FridgeRecipeDto>> findByFridge(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable,
            @Parameter(description = "필터링할 레시피 타입 (USER/YOUTUBE/AI). null이면 USER+YOUTUBE 기본")
            @RequestParam(name = "types", required = false) List<RecipeType> types) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.UNAUTHORIZED);
        }
        List<RecipeType> filterTypes = (types == null || types.isEmpty())
                ? List.of(RecipeType.USER, RecipeType.YOUTUBE)
                : types;

        Long userId = userDetails.getUser().getId();
        Slice<FridgeRecipeDto> result = devFridgeRecipeService.searchByFridgeDev(userId, pageable, filterTypes);
        return ResponseEntity.ok(result);
    }
}
