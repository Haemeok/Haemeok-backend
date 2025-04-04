package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.*;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/recipes")
@RequiredArgsConstructor
public class RecipeController {

    private final RecipeService recipeService;
    private final UserService userService;

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
}
