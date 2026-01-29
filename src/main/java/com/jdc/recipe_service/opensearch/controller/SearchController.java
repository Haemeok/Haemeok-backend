package com.jdc.recipe_service.opensearch.controller;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.type.IngredientType;
import com.jdc.recipe_service.opensearch.service.OpenSearchSuggestionService;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.opensearch.service.OpenSearchService;
import com.jdc.recipe_service.service.IngredientService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Slf4j
@RestController
@RequestMapping("/api/search")
@RequiredArgsConstructor
@Tag(name = "레시피/재료 검색 API", description = "OpenSearch를 이용한 레시피, 재료, 자동완성, 인기 키워드 검색 API입니다.")
public class SearchController {

    private final OpenSearchService searchService;
    private final OpenSearchSuggestionService suggestionService;
    private final IngredientService ingredientService;

    @GetMapping("/recipes")
    @Operation(summary = "레시피 검색", description = "제목, 디시타입, 태그명 기반으로 OpenSearch에서 레시피를 검색합니다. 정렬 기준: createdAt, likeCount")
    public ResponseEntity<Page<RecipeSimpleDto>> searchRecipes(
            @Parameter(description = "검색 조건 (제목, 디시타입, 태그, 영양성분 등)")
            @ModelAttribute RecipeSearchCondition cond,
            @Parameter(
                    name = "sort",
                    description = "정렬 기준 (예: createdAt,DESC 또는 likeCount,DESC)",
                    example = "createdAt,DESC"
            )
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null ? userDetails.getUser().getId() : null;

        Page<RecipeSimpleDto> page = searchService.searchRecipes(cond, pageable, userId);
        return ResponseEntity.ok(page);
    }

    @GetMapping("/recipes/suggest")
    @Operation(summary = "레시피 자동완성", description = "레시피 제목 자동완성 제안 API입니다. prefix 길이는 최소 1자 이상 권장")
    public List<String> suggestRecipes(
            @Parameter(description = "입력 prefix") @RequestParam String prefix,
            @Parameter(description = "제안 최대 개수") @RequestParam(defaultValue = "10") int size) {
        if (prefix.isBlank()) {
            return List.of();
        }
        return suggestionService.suggestRecipeTitles(prefix, size);
    }

    @GetMapping("/keywords/top")
    @Operation(summary = "인기 검색어 조회", description = "전체 검색 키워드 중 누적 검색량 기준 Top N을 반환합니다.")
    public List<String> topKeywords(
            @Parameter(description = "검색어 개수")
            @RequestParam(defaultValue = "10") int size) {
        return suggestionService.getTopSearchKeywords(size);
    }

    @GetMapping("/ingredients")
    @Operation(summary = "재료 검색 (MySQL)", description = "키워드 매핑 방식을 사용하여 재료를 검색합니다.")
    public ResponseEntity<Page<IngredientSummaryDto>> searchIngredients(
            @Parameter(description = "검색어") @RequestParam(required = false) String q,
            @Parameter(description = "카테고리") @RequestParam(required = false) String category,
            @Parameter(description = "정렬 (기본 name)") @RequestParam(required = false) String sort,
            @Parameter(description = "페이지") @RequestParam(defaultValue = "0") int page,
            @Parameter(description = "크기") @RequestParam(defaultValue = "20") int size,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        final int MAX_PAGE_SIZE = 50;
        int safeSize = Math.min(size, MAX_PAGE_SIZE);
        Long userId = (userDetails != null) ? userDetails.getUser().getId() : null;
        String koCategory = null;
        if (category != null && !category.isBlank()) {
            try {
                koCategory = IngredientType.fromCode(category).getKor();
            } catch (IllegalArgumentException e) {
                log.debug("Invalid category code: {}", category);
            }
        }
        Pageable pageable = PageRequest.of(page, safeSize);
        Page<IngredientSummaryDto> result = ingredientService.search(q, koCategory, userId, false, pageable);
        return ResponseEntity.ok(result);
    }
}