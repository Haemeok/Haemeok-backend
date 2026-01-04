package com.jdc.recipe_service.opensearch.controller;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.opensearch.dto.IngredientSearchDto;
import com.jdc.recipe_service.opensearch.service.IngredientSearchService;
import com.jdc.recipe_service.opensearch.service.OpenSearchSuggestionService;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.opensearch.service.OpenSearchService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/search")
@RequiredArgsConstructor
@Tag(name = "레시피/재료 검색 API", description = "OpenSearch를 이용한 레시피, 재료, 자동완성, 인기 키워드 검색 API입니다.")
public class SearchController {

    private final OpenSearchService searchService;
    private final OpenSearchSuggestionService suggestionService;
    private final IngredientSearchService ingredientSearchService;

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
    @Operation(summary = "재료 검색", description = "OpenSearch 인덱스를 이용해 재료명 또는 카테고리로 검색합니다. q, category, sort, dir, page, size 파라미터를 지원합니다.")
    public ResponseEntity<Page<IngredientSearchDto>> searchIngredients(
            @Parameter(description = "재료명 검색어 (prefix) 또는 전체 검색어") @RequestParam(required = false) String q,
            @Parameter(description = "카테고리 필터") @RequestParam(required = false) String category,
            @Parameter(description = "정렬 필드 (예: name, category)") @RequestParam(defaultValue = "name") String sort,
            @Parameter(description = "정렬 방향 (asc 또는 desc)") @RequestParam(defaultValue = "asc") String dir,
            @Parameter(description = "페이지 번호 (0부터 시작)") @RequestParam(defaultValue = "0") int page,
            @Parameter(description = "페이지당 결과 수 (최대 50)") @RequestParam(defaultValue = "20") int size
    ) {
        final int MAX_PAGE_SIZE = 50;
        int safeSize = Math.min(size, MAX_PAGE_SIZE);

        Pageable pageable = PageRequest.of(
                page, safeSize,
                Sort.by(Sort.Direction.fromString(dir), sort)
        );
        Page<IngredientSearchDto> result =
                ingredientSearchService.search(q, category, pageable);
        return ResponseEntity.ok(result);
    }
}