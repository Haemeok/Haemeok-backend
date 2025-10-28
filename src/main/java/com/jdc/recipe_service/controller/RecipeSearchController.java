package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeSearchService;
import com.jdc.recipe_service.util.DeferredResultHolder;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.async.DeferredResult;

import java.util.List;

@RestController
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
@Tag(name = "레시피 검색 API", description = "레시피를 검색하는 API입니다.")
public class RecipeSearchController {

    private final RecipeSearchService recipeSearchService;
    private final RecipeRepository recipeRepository;
    private final DeferredResultHolder deferredResultHolder;

    @GetMapping("/{id}")
    @Operation(summary = "레시피 상세 조회", description = "레시피 ID를 기반으로 상세 정보를 조회합니다.")
    public DeferredResult<ResponseEntity<RecipeDetailDto>> getRecipeDetail(
            @PathVariable("id") Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long currentUserId = userDetails != null
                ? userDetails.getUser().getId()
                : null;

        DeferredResult<ResponseEntity<RecipeDetailDto>> deferredResult = new DeferredResult<>();

        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        boolean ai = Boolean.TRUE.equals(recipe.isAiGenerated());
        RecipeImageStatus status = recipe.getImageStatus();

        if (!ai || status == RecipeImageStatus.READY) {
            RecipeDetailDto fullDto = recipeSearchService.getRecipeDetail(recipeId, currentUserId);
            deferredResult.setResult(ResponseEntity.ok(fullDto));
            return deferredResult;
        }

        deferredResultHolder.add(
                recipeId,
                (DeferredResult<ResponseEntity<?>>)(Object) deferredResult
        );
        return deferredResult;
    }

    @GetMapping("/simple")
    @Operation(summary = "전체 레시피 목록 조회", description = "전체 레시피를 간단한 형태로 조회합니다.")
    public ResponseEntity<Page<RecipeSimpleDto>> getAllSimple(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(hidden = true)
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;

        Page<RecipeSimpleDto> result =
                recipeSearchService.getAllRecipesSimple(userId, pageable);

        return ResponseEntity.ok(result);
    }

    @GetMapping("/by-tag")
    @Operation(summary = "태그 기반 레시피 조회", description = "입력된 태그 이름을 기반으로 레시피를 조회합니다.")
    public ResponseEntity<Page<RecipeSimpleDto>> getByTag(
            @Parameter(description = "조회할 태그 이름 목록") @RequestParam List<String> tags,
            @Parameter(hidden = true)
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;

        return ResponseEntity.ok(
                recipeSearchService.getByTagWithLikeInfo(
                        tags.get(0), userId, pageable
                )
        );
    }

    @GetMapping("/by-dish-type")
    @Operation(summary = "디시타입 기반 레시피 조회", description = "입력된 디시타입을 기준으로 레시피 목록을 반환합니다.")
    public ResponseEntity<Page<RecipeSimpleDto>> byDish(
            @Parameter(description = "디시타입 이름 (예: 볶음, 찜/조림 등)")
            @RequestParam String dishType,
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(hidden = true)
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;

        return ResponseEntity.ok(
                recipeSearchService.getByDishTypeWithLikeInfo(
                        dishType, userId, pageable
                )
        );
    }

    @GetMapping("/search")
    @Operation(summary = "조건 검색", description = "제목, 디시타입, 태그명을 조합하여 레시피를 검색합니다.")
    public ResponseEntity<Page<RecipeSimpleDto>> search(
            @Parameter(description = "검색어 (제목, 설명, 재료명 포함)") @RequestParam(required = false) String q,
            @Parameter(description = "디시타입 (예: 볶음, 찜/조림 등)") @RequestParam(required = false) String dishType,
            @Parameter(description = "태그 이름 목록") @RequestParam(required = false) List<String> tags,
            @Parameter(description = "AI 생성 여부 (true: AI가 만든 레시피만, false: 유저 생성 레시피만)") @RequestParam(required = false) Boolean isAiGenerated,
            @Parameter(description = "최대 허용 원가 (원)") @RequestParam(required = false) Integer maxCost,
            @Parameter(hidden = true) @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;

        Boolean defaultAiFilter = isAiGenerated;

        if (defaultAiFilter == null && (q == null || q.isBlank())) {
            defaultAiFilter = false;
        }

        RecipeSearchCondition cond = new RecipeSearchCondition(q, dishType, tags, defaultAiFilter, maxCost);

        return ResponseEntity.ok(
                recipeSearchService.searchRecipes(cond, pageable, userId)
        );
    }

    @GetMapping("/popular")
    @Operation(summary = "기간별 인기 레시피 목록 조회", description = "최근 좋아요 수 기준 인기 레시피 목록을 조회합니다. 좋아요 수 내림차순 정렬됩니다.")
    public ResponseEntity<Page<RecipeSimpleDto>> getPopularRecipes(
            @Parameter(description = "기간 기준 (weekly, monthly)") @RequestParam(defaultValue = "weekly") String period,
            @Parameter(hidden = true) @PageableDefault(size = 10) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;

        return ResponseEntity.ok(
                recipeSearchService.getPopularRecipes(period, pageable, userId)
        );
    }

    @GetMapping("/budget")
    @Operation(summary = "원가 기준 예산 레시피 목록 조회", description = "총 재료 원가(totalIngredientCost)가 특정 금액 이하인 레시피 목록을 조회합니다. 원가 오름차순 정렬됩니다.")
    public ResponseEntity<Page<RecipeSimpleDto>> getBudgetRecipes(
            @Parameter(description = "최대 허용 원가 (원)") @RequestParam(defaultValue = "10000") Integer maxCost,
            @Parameter(hidden = true) @PageableDefault(size = 10) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;

        return ResponseEntity.ok(
                recipeSearchService.getBudgetRecipes(maxCost, pageable, userId)
        );
    }
}
