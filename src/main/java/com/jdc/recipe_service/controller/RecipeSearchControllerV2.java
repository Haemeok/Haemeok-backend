package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.v2.recipe.*;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeSearchServiceV2;
import com.jdc.recipe_service.service.RecipeStatusService;
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

import java.util.Collections;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/v2/recipes")
@RequiredArgsConstructor
@Tag(name = "레시피 검색 V2 API", description = "성능이 개선된 V2 레시피 API입니다.")
public class RecipeSearchControllerV2 {

    private final RecipeSearchServiceV2 recipeSearchServiceV2;
    private final RecipeStatusService recipeStatusService;
    private final RecipeRepository recipeRepository;
    private final DeferredResultHolder deferredResultHolder;

    @GetMapping("/{id}")
    @Operation(summary = "레시피 상세 조회 (정적)", description = "레시피 ID를 기반으로 정적 상세 정보를 조회합니다.")
    public DeferredResult<ResponseEntity<RecipeDetailStaticDto>> getRecipeDetail(
            @PathVariable("id") Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long currentUserId = userDetails != null ? userDetails.getUser().getId() : null;
        DeferredResult<ResponseEntity<RecipeDetailStaticDto>> deferredResult = new DeferredResult<>();
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));
        boolean ai = Boolean.TRUE.equals(recipe.isAiGenerated());
        RecipeImageStatus status = recipe.getImageStatus();
        if (!ai || status == RecipeImageStatus.READY) {
            RecipeDetailStaticDto staticDto = recipeSearchServiceV2.getRecipeDetail(recipeId, currentUserId);
            deferredResult.setResult(ResponseEntity.ok(staticDto));
            return deferredResult;
        }
        deferredResultHolder.add(recipeId, (DeferredResult<ResponseEntity<?>>)(Object) deferredResult);
        return deferredResult;
    }

    @GetMapping("/{id}/status")
    @Operation(summary = "레시피 상세 상태 정보 조회 (동적)", description = "레시피 ID를 기반으로 사용자 특정 동적 정보(좋아요/즐겨찾기 여부, 나의 평점, 댓글 상태 등)를 조회합니다.")
    public ResponseEntity<RecipeDetailStatusDto> getDetailStatus(
            @PathVariable("id") Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null ? userDetails.getUser().getId() : null;
        Map<Long, RecipeDetailStatusDto> statuses = recipeStatusService.getStatuses(Collections.singletonList(recipeId), userId);
        RecipeDetailStatusDto detailStatus = statuses.get(recipeId);

        if (detailStatus == null) {
            throw new CustomException(ErrorCode.RECIPE_NOT_FOUND);
        }

        return ResponseEntity.ok(detailStatus);
    }

    @PostMapping("/status")
    @Operation(summary = "레시피 상태 정보 배치 조회 (목록용 동적)", description = "레시피 ID 목록으로 동적 정보(좋아요 여부)를 일괄 조회합니다. 응답은 목록 조회용(Simple Status)입니다.")
    public ResponseEntity<Map<Long, RecipeSimpleStatusDto>> getStatuses(
            @RequestBody RecipeStatusRequest request,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null ? userDetails.getUser().getId() : null;

        List<Long> recipeIds = request.getRecipeIds();

        if (recipeIds == null || recipeIds.isEmpty()) {
            return ResponseEntity.ok(Collections.emptyMap());
        }

        Map<Long, RecipeDetailStatusDto> detailStatuses = recipeStatusService.getStatuses(recipeIds, userId);
        Map<Long, RecipeSimpleStatusDto> simpleStatuses = recipeStatusService.convertToSimpleStatus(detailStatuses);

        return ResponseEntity.ok(simpleStatuses);
    }

    @GetMapping("/search")
    @Operation(summary = "조건 검색 (v1 스타일)", description = "제목, 디시타입, 태그명을 조합하여 레시피를 검색합니다.")
    public ResponseEntity<Page<RecipeSimpleStaticDto>> search(
            @RequestParam(required = false) String q,
            @RequestParam(required = false) String dishType,
            @RequestParam(required = false) List<String> tags,
            @RequestParam(required = false) Boolean isAiGenerated,
            @RequestParam(required = false) Integer maxCost,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null ? userDetails.getUser().getId() : null;

        Boolean defaultAiFilter = isAiGenerated;

        if (defaultAiFilter == null && (q == null || q.isBlank())) {
            defaultAiFilter = false;
        }

        RecipeSearchCondition cond = new RecipeSearchCondition(q, dishType, tags, defaultAiFilter, maxCost);
        Page<RecipeSimpleStaticDto> page = recipeSearchServiceV2.searchRecipes(cond, pageable, userId);
        return ResponseEntity.ok(page);
    }

    @GetMapping("/popular")
    @Operation(summary = "기간별 인기 레시피 목록 조회 (정적)", description = "최근 좋아요 수 기준 인기 레시피 목록을 조회합니다. **동적 정보는 /status 엔드포인트를 이용해야 합니다.**")
    public ResponseEntity<Page<RecipeSimpleStaticDto>> getPopularRecipesStatic(
            @Parameter(description = "기간 기준 (weekly, monthly)") @RequestParam(defaultValue = "weekly") String period,
            @PageableDefault(size = 10) Pageable pageable
    ) {
        Page<RecipeSimpleStaticDto> page = recipeSearchServiceV2.getPopularRecipesStatic(period, pageable);
        return ResponseEntity.ok(page);
    }


    @GetMapping("/budget")
    @Operation(summary = "원가 기준 예산 레시피 목록 조회 (정적)", description = "총 재료 원가(totalIngredientCost)가 특정 금액 이하인 레시피 목록을 조회합니다. **동적 정보는 /status 엔드포인트를 이용해야 합니다.**")
    public ResponseEntity<Page<RecipeSimpleStaticDto>> getBudgetRecipesStatic(
            @Parameter(description = "최대 허용 원가 (원)") @RequestParam(defaultValue = "10000") Integer maxCost,
            @PageableDefault(size = 10) Pageable pageable
    ) {
        Page<RecipeSimpleStaticDto> page = recipeSearchServiceV2.getBudgetRecipesStatic(maxCost, pageable);
        return ResponseEntity.ok(page);
    }
}