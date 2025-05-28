package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.recipe.MyRecipeSummaryDto;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponseItem;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.domain.dto.user.UserPatchDTO;
import com.jdc.recipe_service.domain.dto.user.UserResponseDTO;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.UserService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

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
    /*** 다른 사람 작성 레시피 조회 ***/
    @GetMapping("/{userId}/recipes")
    public ResponseEntity<Page<MyRecipeSummaryDto>> getUserRecipes(
            @PathVariable Long userId,
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable) {

        // 로그인 되어 있으면 viewerId, 아니면 null
        Long viewerId = (userDetails != null)
                ? userDetails.getUser().getId()
                : null;

        Page<MyRecipeSummaryDto> page =
                userService.getUserRecipes(userId, viewerId, pageable);

        return ResponseEntity.ok(page);
    }

    // Presign URL 발급 ---
    @GetMapping("/{userId}/profile-image/presign")
    public ResponseEntity<PresignedUrlResponseItem> presignProfileImage(
            @PathVariable Long userId,
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        if (!userDetails.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.USER_ACCESS_DENIED);
        }

        PresignedUrlResponseItem presign = userService.generateProfileImagePresign(userId);
        return ResponseEntity.ok(presign);
    }

    // 유저 정보 & imageKey 업데이트
    @PatchMapping("/{userId}")
    public ResponseEntity<UserResponseDTO> patchUser(
            @PathVariable Long userId,
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Valid @RequestBody UserPatchDTO dto) {

        if (userDetails == null) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        if (!userDetails.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.USER_ACCESS_DENIED);
        }

        UserResponseDTO updated = userService.updateUser(userId, dto);
        return ResponseEntity.ok(updated);
    }

}