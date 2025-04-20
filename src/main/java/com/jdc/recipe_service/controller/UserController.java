package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.MyRecipeSummaryDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.service.UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import static org.springframework.data.domain.Sort.Direction.DESC;

@RestController
@RequestMapping("/api/users")
@RequiredArgsConstructor
public class UserController {

    private final UserService userService;

    // 공개 프로필 조회
    @GetMapping("/{userId}")
    public ResponseEntity<UserDto> getUserProfile(@PathVariable Long userId) {
        UserDto dto = userService.getPublicProfile(userId);
        return ResponseEntity.ok(dto);
    }
    // 작성 레시피 조회 (누구나 가능)
    @GetMapping("/{userId}/recipes")
    public ResponseEntity<Page<MyRecipeSummaryDto>> getUserRecipes(
            @PathVariable Long userId,
            @PageableDefault(size = 10, sort = "createdAt", direction = DESC) Pageable pageable) {

        Page<MyRecipeSummaryDto> page = userService.getMyRecipes(userId, pageable);
        return ResponseEntity.ok(page);
    }
}
