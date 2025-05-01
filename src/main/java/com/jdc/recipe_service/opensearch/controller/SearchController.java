package com.jdc.recipe_service.opensearch.controller;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.opensearch.service.OpenSearchService;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/search")
public class SearchController {

    private final OpenSearchService searchService;

    public SearchController(OpenSearchService searchService) {
        this.searchService = searchService;
    }

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

    /** 재료 검색 */
    @GetMapping("/ingredients")
    public ResponseEntity<Page<IngredientSummaryDto>> searchIngredients(
            @RequestParam(required = false) String q,
            @RequestParam(required = false) String category,
            @RequestParam(required = false) Boolean inFridge,
            @PageableDefault(size = 20, sort = "name", direction = Sort.Direction.ASC)
            Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = userDetails != null ? userDetails.getUser().getId() : null;
        Page<IngredientSummaryDto> page = searchService.searchIngredients(q, category, inFridge, userId, pageable);
        return ResponseEntity.ok(page);
    }
}
