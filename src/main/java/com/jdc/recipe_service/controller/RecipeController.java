package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeRatingService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.UserService;
import lombok.RequiredArgsConstructor;
import org.apache.coyote.BadRequestException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
public class RecipeController {

    private final RecipeService recipeService;
    private final UserService userService;
    private final RecipeRatingService recipeRatingService;

    @PostMapping
    public ResponseEntity<Long> createRecipe(@RequestBody RecipeCreateRequestDto requestDto,
                                             Authentication authentication) {
//        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
//        Long recipeId = recipeService.createRecipe(requestDto, userId);
//        return ResponseEntity.ok(recipeId);
        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : userService.getGuestUser().getId(); // ✅ 비회원 fallback
        Long recipeId = recipeService.createRecipe(requestDto, userId);
        return ResponseEntity.ok(recipeId);
    }

    @GetMapping("/{id}")
    public ResponseEntity<RecipeDetailDto> getRecipeDetail(@PathVariable("id") Long recipeId,
                                                           Authentication authentication) {
//        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : userService.getGuestUser().getId(); // ✅ 비회원 fallback
        RecipeDetailDto recipeDetail = recipeService.getRecipeDetail(recipeId, userId);
        return ResponseEntity.ok(recipeDetail);
    }

    @PutMapping("/{recipeId}")
    public ResponseEntity<Map<String, Long>> updateRecipe(@PathVariable Long recipeId,
                                                          @RequestBody RecipeCreateRequestDto dto,
                                                          Authentication authentication) {
//        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : userService.getGuestUser().getId(); // ✅ 비회원 fallback
        Long updatedId = recipeService.updateRecipe(recipeId, userId, dto);
        return ResponseEntity.ok(Map.of("id", updatedId));
    }

    @DeleteMapping("/{recipeId}")
    public ResponseEntity<String> deleteRecipe(@PathVariable Long recipeId,
                                               Authentication authentication) {
//        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : userService.getGuestUser().getId(); // ✅ 비회원 fallback
        try {
            recipeService.deleteRecipe(recipeId, userId);
            return ResponseEntity.ok("레시피가 성공적으로 삭제되었습니다.");
        } catch (RuntimeException ex) {
            return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(ex.getMessage());
        }
    }

    @GetMapping("/simple")
    public ResponseEntity<List<RecipeSimpleDto>> getAllSimpleRecipes(Authentication authentication) {
//        Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : userService.getGuestUser().getId(); // ✅ 비회원 fallback
        return ResponseEntity.ok(recipeService.getAllRecipesSimple(userId));
    }

    @GetMapping("/by-tag")
    public ResponseEntity<Page<RecipeSimpleDto>> getRecipesByTag(
            @RequestParam String tag,
            @RequestParam(required = false) Long userId, // 로그인 유저 ID (nullable)
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable
    ) throws BadRequestException {
        Page<RecipeSimpleDto> recipes = recipeService.getByTagWithLikeInfo(tag, userId, pageable);
        return ResponseEntity.ok(recipes);
    }

    @GetMapping("/by-dish-type")
    public ResponseEntity<Page<RecipeSimpleDto>> getRecipesByDishType(
            @RequestParam String dishType, // 🔄 "FRY"
            @RequestParam(required = false) Long userId,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable
    ) {
        Page<RecipeSimpleDto> recipes = recipeService.getByDishTypeWithLikeInfo(dishType, userId, pageable);
        return ResponseEntity.ok(recipes);
    }

    @GetMapping("/dish-types")
    public ResponseEntity<List<DishTypeDto>> getAllDishTypes() {
        List<DishTypeDto> types = Arrays.stream(DishType.values())
                .map(type -> new DishTypeDto(type.name(), type.getDisplayName()))
                .toList();

        return ResponseEntity.ok(types);
    }

    // ✔ 평점 등록 또는 수정
    @PostMapping("/{id}/rating")
    public ResponseEntity<?> rateRecipe(@PathVariable Long id,
                                        @RequestBody RecipeRatingRequestDto dto,
                                        Authentication authentication) {
//    Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : userService.getGuestUser().getId(); // ✅ 비회원 fallback
        recipeRatingService.rateRecipe(id, userId, dto);
        return ResponseEntity.ok(Map.of("message", "평점 등록 완료"));
    }

    // ✔ 내가 준 평점 조회
    @GetMapping("/{id}/rating")
    public ResponseEntity<?> getMyRating(@PathVariable Long id,
                                         Authentication authentication) {
//    Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : userService.getGuestUser().getId(); // ✅ 비회원 fallback
        Double rating = recipeRatingService.getMyRating(id, userId);
        return ResponseEntity.ok(Map.of("rating", rating));
    }

    @DeleteMapping("/{id}/rating")
    public ResponseEntity<?> deleteRating(@PathVariable Long id,
                                          Authentication authentication) {
        //    Long userId = ((CustomUserDetails) authentication.getPrincipal()).getUser().getId();
        Long userId = (authentication != null && authentication.isAuthenticated())
                ? ((CustomUserDetails) authentication.getPrincipal()).getUser().getId()
                : userService.getGuestUser().getId(); // ✅ 비회원 fallback
        recipeRatingService.deleteRating(id, userId);
        return ResponseEntity.ok(Map.of("message", "평점 삭제 완료"));
    }
}
