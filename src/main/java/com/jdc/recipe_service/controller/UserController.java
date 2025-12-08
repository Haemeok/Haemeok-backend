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
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "사용자 공개 API", description = "사용자 프로필 및 작성한 레시피 등을 조회하거나 프로필 이미지를 업데이트하는 API입니다.")
public class UserController {

    private final UserService userService;

    @GetMapping("/{userId}")
    @Operation(summary = "공개 사용자 프로필 조회", description = "특정 사용자의 공개 프로필 정보를 반환합니다.")
    public ResponseEntity<UserDto> getUserProfile(@PathVariable Long userId) {
        UserDto dto = userService.getPublicProfile(userId);
        return ResponseEntity.ok(dto);
    }

    @GetMapping("/{userId}/recipes")
    @Operation(summary = "사용자가 작성한 레시피 목록 조회", description = "특정 사용자가 작성한 레시피 목록을 페이징하여 조회합니다.")
    public ResponseEntity<Page<MyRecipeSummaryDto>> getUserRecipes(
            @PathVariable Long userId,
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable) {

        Long viewerId = (userDetails != null)
                ? userDetails.getUser().getId()
                : null;

        Page<MyRecipeSummaryDto> page =
                userService.getUserRecipes(userId, viewerId, pageable);

        return ResponseEntity.ok(page);
    }

    @GetMapping("/{userId}/profile-image/presign")
    @Operation(summary = "프로필 이미지 Presigned URL 발급", description = "S3에 업로드할 수 있도록 Presigned URL을 발급받습니다.")
    public ResponseEntity<PresignedUrlResponseItem> presignProfileImage(
            @PathVariable Long userId,
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @RequestParam(required = false) String contentType
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        if (!userDetails.getUser().getId().equals(userId)) {
            throw new CustomException(ErrorCode.USER_ACCESS_DENIED);
        }

        PresignedUrlResponseItem presign = userService.generateProfileImagePresign(userId,contentType);
        return ResponseEntity.ok(presign);
    }

    @PatchMapping("/{userId}")
    @Operation(summary = "사용자 정보 수정", description = "사용자의 닉네임, 소개글, 이미지 키 등을 수정합니다.")
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