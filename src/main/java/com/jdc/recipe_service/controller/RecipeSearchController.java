package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.dto.AiRecipeFilter;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeRecommendationService;
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
    private final RecipeRecommendationService recipeRecommendationService;

    @GetMapping("/{id}")
    @Operation(summary = "레시피 상세 조회", description = "레시피 ID를 기반으로 상세 정보를 조회합니다.")
    public DeferredResult<ResponseEntity<RecipeDetailDto>> getRecipeDetail(
            @DecodeId("id") Long recipeId,
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

    @GetMapping("/search")
    @Operation(summary = "조건 검색", description = "제목, 디시타입, 태그명을 조합하여 레시피를 검색합니다.")
    public ResponseEntity<Page<RecipeSimpleDto>> search(
            @Parameter(description = "검색 조건 (제목, 디시타입, 태그, AI필터, 가격, 영양성분 등)")
            @ModelAttribute RecipeSearchCondition cond,
            @Parameter(hidden = true) @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;

        if (cond.getAiFilter() == null) {
            cond.setAiFilter(AiRecipeFilter.USER_ONLY);
        }

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

    @GetMapping("/{id}/recommendations")
    @Operation(summary = "상세 페이지 하단 추천 레시피 조회")
    public ResponseEntity<List<RecipeSimpleStaticDto>> getRecommendations(
            @DecodeId("id") Long recipeId,
            @RequestParam(defaultValue = "10") int size
    ) {
        List<RecipeSimpleStaticDto> recommendations = recipeRecommendationService.getRecommendations(recipeId, size);
        return ResponseEntity.ok(recommendations);
    }
}
