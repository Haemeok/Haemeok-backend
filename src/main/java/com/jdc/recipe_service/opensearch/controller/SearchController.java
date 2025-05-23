package com.jdc.recipe_service.opensearch.controller;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.opensearch.dto.IngredientSearchDto;
import com.jdc.recipe_service.opensearch.service.IngredientSearchService;
import com.jdc.recipe_service.opensearch.service.OpenSearchSuggestionService;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.opensearch.service.OpenSearchService;
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
public class SearchController {

    private final OpenSearchService searchService;
    private final OpenSearchSuggestionService suggestionService;
    private final IngredientSearchService ingredientSearchService;

    /** 레시피 검색 */
    @GetMapping("/recipes")
    public ResponseEntity<Page<RecipeSimpleDto>> searchRecipes(
            @RequestParam(required = false) String q,
            @RequestParam(required = false) String dishType,
            @RequestParam(required = false) List<String> tagNames,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null ? userDetails.getUser().getId() : null;
        RecipeSearchCondition cond = new RecipeSearchCondition(q, dishType, tagNames);
        Page<RecipeSimpleDto> page = searchService.searchRecipes(cond, pageable, userId);
        return ResponseEntity.ok(page);
    }

    /** 레시피 제목 자동완성 제안 */
    @GetMapping("/recipes/suggest")
    public List<String> suggestRecipes(@RequestParam String prefix,
                                       @RequestParam(defaultValue = "10") int size) {
        if (prefix.isBlank()) {
            return List.of();
        }
        return suggestionService.suggestRecipeTitles(prefix, size);
    }

    /** 🔥 전체 누적 인기 검색어 Top N */
    @GetMapping("/keywords/top")
    public List<String> topKeywords(
            @RequestParam(defaultValue = "10") int size) {
        return suggestionService.getTopSearchKeywords(size);
    }

    /**
     * 색인 기반 재료 검색
     * q, category, sort, dir, page, size 지원
     */
    @GetMapping("/ingredients")
    public ResponseEntity<Page<IngredientSearchDto>> searchIngredients(
            @RequestParam(required = false) String q,
            @RequestParam(required = false) String category,
            @RequestParam(defaultValue = "name") String sort,
            @RequestParam(defaultValue = "asc") String dir,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size
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
