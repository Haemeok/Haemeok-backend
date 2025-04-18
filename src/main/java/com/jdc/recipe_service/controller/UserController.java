package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.MyRecipeSummaryDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.user.UserRequestDTO;
import com.jdc.recipe_service.domain.dto.user.UserResponseDTO;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.UserService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.springframework.data.domain.Sort.Direction.DESC;

@RestController
@RequestMapping("/api/users")
@RequiredArgsConstructor
@Validated
public class UserController {

    private final UserService userService;

    // 유저 생성 API
    @PostMapping
    public ResponseEntity<UserResponseDTO> createUser(@Valid @RequestBody UserRequestDTO dto) {
        UserResponseDTO response = userService.createUser(dto);
        return new ResponseEntity<>(response, HttpStatus.CREATED);
    }

    // 유저 조회 API (단일)
    @GetMapping("/{id}")
    public ResponseEntity<UserResponseDTO> getUser(@PathVariable Long id) {
        UserResponseDTO response = userService.getUser(id);
        return ResponseEntity.ok(response);
    }

    // 모든 유저 조회 API
    @GetMapping
    public ResponseEntity<List<UserResponseDTO>> getAllUsers() {
        List<UserResponseDTO> users = userService.getAllUsers();
        return ResponseEntity.ok(users);
    }


    // 유저 업데이트 API
    @PutMapping("/{id}")
    public ResponseEntity<UserResponseDTO> updateUser(@PathVariable Long id, @Valid @RequestBody UserRequestDTO dto) {
        UserResponseDTO updatedUser = userService.updateUser(id, dto);
        return ResponseEntity.ok(updatedUser);
    }

    // 유저 삭제 API
    @DeleteMapping("/{id}")
    public ResponseEntity<String> deleteUser(@PathVariable Long id) {
        userService.deleteUser(id);
        return ResponseEntity.ok("사용자가 삭제되었습니다.");
    }

    /*** 내 즐겨찾기 조회 ***/
    @GetMapping("/favorites")
    public ResponseEntity<Page<RecipeSimpleDto>> getMyFavorites(
            Authentication authentication,
            @PageableDefault(size = 10, sort = "createdAt", direction = DESC) Pageable pageable) {

        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.ok(Page.empty(pageable));
        }
        Long currentUserId = ((CustomUserDetails) authentication.getPrincipal())
                .getUser().getId();
        Page<RecipeSimpleDto> page = userService.getFavoriteRecipesByUser(currentUserId, currentUserId, pageable);
        return ResponseEntity.ok(page);
    }

    /*** 내 작성 레시피 조회 ***/
    @GetMapping("/recipes")
    public ResponseEntity<Page<MyRecipeSummaryDto>> getMyRecipes(
            Authentication authentication,
            @PageableDefault(size = 10, sort = "createdAt", direction = DESC) Pageable pageable) {

        if (authentication == null || !authentication.isAuthenticated()) {
            return ResponseEntity.ok(Page.empty(pageable));
        }
        Long currentUserId = ((CustomUserDetails) authentication.getPrincipal())
                .getUser().getId();
        Page<MyRecipeSummaryDto> page = userService.getMyRecipes(currentUserId, pageable);
        return ResponseEntity.ok(page);
    }

}
