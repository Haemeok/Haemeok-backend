package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.v2.RecipeDetailStaticDto;
import com.jdc.recipe_service.domain.dto.recipe.v2.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.dto.recipe.v2.RecipeStatusDto;
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

    @PostMapping("/status")
    @Operation(summary = "레시피 상태 정보 조회 (동적)", description = "레시피 ID 목록으로 동적 정보(좋아요, 댓글 등)를 일괄 조회합니다.")
    public ResponseEntity<Map<Long, RecipeStatusDto>> getStatuses(
            @RequestBody List<Long> recipeIds,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null ? userDetails.getUser().getId() : null;
        if (recipeIds == null || recipeIds.isEmpty()) {
            return ResponseEntity.ok(Collections.emptyMap());
        }
        Map<Long, RecipeStatusDto> statuses = recipeStatusService.getStatuses(recipeIds, userId);
        return ResponseEntity.ok(statuses);
    }

    @GetMapping("/search")
    @Operation(summary = "조건 검색 (v1 스타일)", description = "제목, 디시타입, 태그명을 조합하여 레시피를 검색합니다.")
    public ResponseEntity<Page<RecipeSimpleStaticDto>> search(
            @RequestParam(required = false) String q,
            @RequestParam(required = false) String dishType,
            @RequestParam(required = false) List<String> tagNames,
            @RequestParam(required = false) Boolean isAiGenerated,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null ? userDetails.getUser().getId() : null;
        RecipeSearchCondition cond = new RecipeSearchCondition(q, dishType, tagNames, isAiGenerated);
        Page<RecipeSimpleStaticDto> page = recipeSearchServiceV2.searchRecipes(cond, pageable, userId);
        return ResponseEntity.ok(page);
    }
}