package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.user.UserRequestDTO;
import com.jdc.recipe_service.domain.dto.user.UserResponseDTO;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.UserService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/me")
@RequiredArgsConstructor
public class MyAccountController {

    private final UserService userService;

    //  내 정보 조회
    @GetMapping
    public ResponseEntity<UserResponseDTO> getMyInfo(
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = Long.valueOf(userDetails.getUsername());
        return ResponseEntity.ok(userService.getUser(userId));
    }

    //  내 프로필 수정
    @PutMapping
    public ResponseEntity<UserResponseDTO> updateMyProfile(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Valid @RequestBody UserRequestDTO dto) {
        Long userId = Long.valueOf(userDetails.getUsername());
        return ResponseEntity.ok(userService.updateUser(userId, dto));
    }

    //  내 계정 삭제 (하드 삭제)
    @DeleteMapping
    public ResponseEntity<String> deleteMyAccount(
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = Long.valueOf(userDetails.getUsername());
        userService.deleteUser(userId);
        return ResponseEntity.ok("계정이 삭제되었습니다.");
    }

    // 내 즐겨찾기 조회
    @GetMapping("/favorites")
    public ResponseEntity<List<RecipeSimpleDto>> getMyFavorites(
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        Long userId = Long.valueOf(userDetails.getUsername());
        List<RecipeSimpleDto> favorites = userService.getFavoriteRecipesByUser(userId);
        return ResponseEntity.ok(favorites);
    }
}
