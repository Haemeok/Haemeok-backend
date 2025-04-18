package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.recipe.*;
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

import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
public class RecipeController {

    private final RecipeService recipeService;
    private final RecipeRatingService recipeRatingService;

    @PostMapping
    public ResponseEntity<Long> createRecipe(@RequestBody @Valid RecipeCreateRequestDto requestDto,
                                             Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Long recipeId = recipeService.createRecipe(requestDto, userId);
        return ResponseEntity.ok(recipeId);
    }

    @GetMapping("/{recipeId}")
    public ResponseEntity<RecipeDetailDto> getRecipeDetail(@PathVariable("recipeId") Long recipeId,
                                                           Authentication authentication) {
        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : null;
        RecipeDetailDto recipeDetail = recipeService.getRecipeDetail(recipeId, userId);
        return ResponseEntity.ok(recipeDetail);
    }

    @PutMapping("/{recipeId}")
    public ResponseEntity<Map<String, Long>> updateRecipe(@PathVariable Long recipeId,
                                                          @RequestBody RecipeCreateRequestDto dto,
                                                          Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Long updatedId = recipeService.updateRecipe(recipeId, userId, dto);
        return ResponseEntity.ok(Map.of("id", updatedId));
    }

    @DeleteMapping("/{recipeId}")
    public ResponseEntity<String> deleteRecipe(@PathVariable Long recipeId,
                                               Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        try {
            recipeService.deleteRecipe(recipeId, userId);
            return ResponseEntity.ok("레시피가 성공적으로 삭제되었습니다.");
        } catch (RuntimeException ex) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(ex.getMessage());
        }
    }

    // ✅ 1. 유저 전용 레시피 생성
    @PostMapping("/user")
    public ResponseEntity<?> createUserRecipe(@RequestBody RecipeUserCreateRequestDto dto,
                                              Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Long recipeId = recipeService.createUserRecipe(dto, userId);
        return ResponseEntity.ok(Map.of("recipeId", recipeId));
    }

    // ✅ 2. 유저 전용 레시피 수정
    @PutMapping("/user/{id}")
    public ResponseEntity<?> updateUserRecipe(@PathVariable Long id,
                                              @RequestBody RecipeUserCreateRequestDto dto,
                                              Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        recipeService.updateUserRecipe(id, userId, dto);
        return ResponseEntity.ok(Map.of("recipeId", id));
    }

    // ✅ 3. 유저 전용 레시피 삭제
    @DeleteMapping("/user/{id}")
    public ResponseEntity<?> deleteUserRecipe(@PathVariable Long id,
                                              Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        recipeService.deleteRecipe(id, userId); // validateOwnership 내부 호출
        return ResponseEntity.ok(Map.of("deletedRecipeId", id));
    }


    @GetMapping("/simple")
    public ResponseEntity<List<RecipeSimpleDto>> getAllSimpleRecipes(Authentication authentication) {
        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        return ResponseEntity.ok(recipeService.getAllRecipesSimple(userId));
    }

    @PostMapping("/search")
    public ResponseEntity<Page<RecipeSimpleDto>> searchRecipes(
            @RequestBody RecipeSearchCondition condition,
            Pageable pageable,
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        Long userId = (userDetails != null) ? userDetails.getUser().getId() : null;
        Page<RecipeSimpleDto> result = recipeService.searchRecipes(condition, pageable, userId);
        return ResponseEntity.ok(result);
    }

    @GetMapping("/by-tag")
    public ResponseEntity<Page<RecipeSimpleDto>> getRecipesByTag(
            @RequestParam String tag,
            Authentication authentication,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable
    ) throws BadRequestException {

        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : null;
        Page<RecipeSimpleDto> recipes =
                recipeService.getByTagWithLikeInfo(tag, userId, pageable);

        return ResponseEntity.ok(recipes);
    }

    @GetMapping("/by-dish-type")
    public ResponseEntity<Page<RecipeSimpleDto>> getRecipesByDishType(
            @RequestParam String dishType,  // ex. "FRY"
            Authentication authentication,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable
    ) {
        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : null;
        Page<RecipeSimpleDto> recipes =
                recipeService.getByDishTypeWithLikeInfo(dishType, userId, pageable);

        return ResponseEntity.ok(recipes);
    }

    // ✔ 평점 등록 또는 수정
    @PostMapping("/{id}/rating")
    public ResponseEntity<?> rateRecipe(@PathVariable Long id,
                                        @RequestBody RecipeRatingRequestDto dto,
                                        Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        recipeRatingService.rateRecipe(id, userId, dto);
        return ResponseEntity.ok(Map.of("message", "평점 등록 완료"));
    }

    // ✔ 내가 준 평점 조회
    @GetMapping("/{id}/rating")
    public ResponseEntity<?> getMyRating(@PathVariable Long id,
                                         Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Double rating = recipeRatingService.getMyRating(id, userId);
        return ResponseEntity.ok(Map.of("rating", rating));
    }

    @DeleteMapping("/{id}/rating")
    public ResponseEntity<?> deleteRating(@PathVariable Long id,
                                          Authentication authentication) {
        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.status(HttpStatus.UNAUTHORIZED).build();
        }
        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        recipeRatingService.deleteRating(id, userId);
        return ResponseEntity.ok(Map.of("message", "평점 삭제 완료"));
    }
}
