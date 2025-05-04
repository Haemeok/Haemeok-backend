package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.RecipeDetailDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeSearchService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
public class RecipeSearchController {
    private final RecipeSearchService recipeService;

    // 레시피 단건 조회 (읽기 전용, 선택 인증)
    @GetMapping("/{recipeId}")
    public ResponseEntity<RecipeDetailDto> getRecipe(
            @PathVariable Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = (userDetails != null)
                ? userDetails.getUser().getId()
                : null;
        return ResponseEntity.ok(
                recipeService.getRecipeDetail(recipeId, userId));
    }

    // 전체 간단 조회 (읽기 전용)
    @GetMapping("/simple")
    public ResponseEntity<Page<RecipeSimpleDto>> getAllSimple(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable) {

        Long userId = (userDetails != null)
                ? userDetails.getUser().getId()
                : null;

        Page<RecipeSimpleDto> result = recipeService.getAllRecipesSimple(userId, pageable);
        return ResponseEntity.ok(result);
    }

    // 태그로 조회
    @GetMapping("/by-tag")
    public ResponseEntity<Page<RecipeSimpleDto>> getByTag(
            @RequestParam List<String> tagNames,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = (userDetails != null) ? userDetails.getUser().getId() : null;
        // 서비스 호출 시 첫 번째 태그만 넘긴다
        return ResponseEntity.ok(
                recipeService.getByTagWithLikeInfo(tagNames.get(0), userId, pageable)
        );
    }

    // 디시타입으로 조회
    @GetMapping("/by-dish-type")
    public ResponseEntity<Page<RecipeSimpleDto>> byDish(
            @RequestParam String dishType,
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable
    ) {
        Long userId = userDetails != null
                ? userDetails.getUser().getId()
                : null;
        return ResponseEntity.ok(
                recipeService.getByDishTypeWithLikeInfo(dishType, userId, pageable));
    }

    // (검색, 태그, 디시타입) 복합 조건 조회 (읽기 전용)
    @GetMapping("/search")
    public ResponseEntity<Page<RecipeSimpleDto>> search(
            @RequestParam(required = false) String q,
            @RequestParam(required = false) String dishType,
            @RequestParam(required = false) List<String> tagNames,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = (userDetails != null)
                ? userDetails.getUser().getId()
                : null;

        RecipeSearchCondition cond = new RecipeSearchCondition(q, dishType, tagNames);
        return ResponseEntity.ok(
                recipeService.searchRecipes(cond, pageable, userId));
    }
}
