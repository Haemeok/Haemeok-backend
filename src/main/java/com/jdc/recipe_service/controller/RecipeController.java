package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeRatingService;
import com.jdc.recipe_service.service.RecipeService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.apache.coyote.BadRequestException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;

import org.springframework.web.server.ResponseStatusException;
import org.springframework.http.HttpStatus;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
public class RecipeController {

    private final RecipeService recipeService;
    private final RecipeRatingService recipeRatingService;

    // 1) 레시피 생성 (인증 필수)
    @PostMapping
    public ResponseEntity<Long> createRecipe(
            @RequestBody @Valid RecipeCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        return ResponseEntity.ok(recipeService.createRecipe(dto, userId));
    }

    // 2) 레시피 단건 조회 (읽기 전용, 선택 인증)
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

    // 3) 레시피 수정 (인증 필수)
    @PutMapping("/{recipeId}")
    public ResponseEntity<Map<String, Long>> updateRecipe(
            @PathVariable Long recipeId,
            @RequestBody RecipeCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        Long updatedId = recipeService.updateRecipe(recipeId, userId, dto);
        return ResponseEntity.ok(Map.of("id", updatedId));
    }

    // 4) 레시피 삭제 (인증 필수)
    @DeleteMapping("/{recipeId}")
    public ResponseEntity<String> deleteRecipe(
            @PathVariable Long recipeId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        try {
            recipeService.deleteRecipe(recipeId, userId);
            return ResponseEntity.ok("레시피가 성공적으로 삭제되었습니다.");
        } catch (RuntimeException ex) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                    .body(ex.getMessage());
        }
    }

    // 5) 유저 전용 레시피 생성·수정·삭제 (인증 필수)
    @PostMapping("/user")
    public ResponseEntity<?> createUserRecipe(
            @RequestBody RecipeUserCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        Long recipeId = recipeService.createUserRecipe(dto, userId);
        return ResponseEntity.ok(Map.of("recipeId", recipeId));
    }

    @PutMapping("/user/{id}")
    public ResponseEntity<?> updateUserRecipe(
            @PathVariable Long id,
            @RequestBody RecipeUserCreateRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        recipeService.updateUserRecipe(id, userId, dto);
        return ResponseEntity.ok(Map.of("recipeId", id));
    }

    @DeleteMapping("/user/{id}")
    public ResponseEntity<?> deleteUserRecipe(
            @PathVariable Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        recipeService.deleteRecipe(id, userId);
        return ResponseEntity.ok(Map.of("deletedRecipeId", id));
    }

    // 6) 전체 간단 조회 (읽기 전용)
    @GetMapping("/simple")
    public ResponseEntity<List<RecipeSimpleDto>> getAllSimple(
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = (userDetails != null)
                ? userDetails.getUser().getId()
                : null;
        return ResponseEntity.ok(
                recipeService.getAllRecipesSimple(userId));
    }

    // 7) 검색, 태그, 디시타입 조회 (읽기 전용)
    @PostMapping("/search")
    public ResponseEntity<Page<RecipeSimpleDto>> search(
            @RequestBody RecipeSearchCondition cond,
            Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = (userDetails != null)
                ? userDetails.getUser().getId()
                : null;
        return ResponseEntity.ok(
                recipeService.searchRecipes(cond, pageable, userId));
    }

    @GetMapping("/by-tag")
    public ResponseEntity<Page<RecipeSimpleDto>> getByTag(
            @RequestParam String tag,
            Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        Long userId = (userDetails != null)
                ? userDetails.getUser().getId()
                : null;
        return ResponseEntity.ok(
                recipeService.getByTagWithLikeInfo(tag, userId, pageable));
    }

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

    @GetMapping("/dish-types")
    public ResponseEntity<List<DishTypeDto>> getAllDishTypes() {
        var types = Arrays.stream(DishType.values())
                .map(t -> new DishTypeDto(t.name(), t.getDisplayName()))
                .toList();
        return ResponseEntity.ok(types);
    }

    // 8) 평점 CRUD (인증 필수)
    @PostMapping("/{id}/rating")
    public ResponseEntity<?> rateRecipe(
            @PathVariable Long id,
            @RequestBody RecipeRatingRequestDto dto,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        recipeRatingService.rateRecipe(id, userId, dto);
        return ResponseEntity.ok(Map.of("message", "평점 등록 완료"));
    }

    @GetMapping("/{id}/rating")
    public ResponseEntity<?> getMyRating(
            @PathVariable Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        Double rating = recipeRatingService.getMyRating(id, userId);
        return ResponseEntity.ok(Map.of("rating", rating));
    }

    @DeleteMapping("/{id}/rating")
    public ResponseEntity<?> deleteRating(
            @PathVariable Long id,
            @AuthenticationPrincipal CustomUserDetails userDetails) {

        if (userDetails == null) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = userDetails.getUser().getId();
        recipeRatingService.deleteRating(id, userId);
        return ResponseEntity.ok(Map.of("message", "평점 삭제 완료"));
    }
}
