package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.calendar.CookingStreakDto;
import com.jdc.recipe_service.domain.dto.recipe.MyRecipeSummaryDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.user.UserPatchDTO;
import com.jdc.recipe_service.domain.dto.user.UserResponseDTO;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.CookingRecordService;
import com.jdc.recipe_service.service.UserService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
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

import static org.springframework.data.domain.Sort.Direction.DESC;

@RestController
@RequestMapping("/api/me")
@RequiredArgsConstructor
@Tag(name = "내 계정 API", description = "내 정보, 즐겨찾기, 작성한 레시피, 요리 기록 등을 조회/수정하는 API입니다.")
public class MyAccountController {

    private final UserService userService;
    private final CookingRecordService cookingService;

    @GetMapping
    @Operation(summary = "내 정보 조회", description = "현재 로그인한 사용자의 정보를 조회합니다.")
    public ResponseEntity<UserResponseDTO> getMyInfo(
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        Long userId = Long.valueOf(userDetails.getUsername());
        return ResponseEntity.ok(userService.getUser(userId));
    }

    @PatchMapping
    @Operation(summary = "내 프로필 수정", description = "닉네임, 소개 등 내 프로필 정보를 수정합니다.")
    public ResponseEntity<UserResponseDTO> patchMyProfile(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @io.swagger.v3.oas.annotations.parameters.RequestBody(description = "수정할 프로필 정보")
            @Valid @RequestBody UserPatchDTO dto) throws CustomException {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        Long userId = Long.valueOf(userDetails.getUsername());
        UserResponseDTO updated = userService.updateUser(userId, dto);
        return ResponseEntity.ok(updated);
    }

    @DeleteMapping
    @Operation(summary = "내 계정 삭제", description = "현재 로그인한 사용자 계정을 완전히 삭제합니다.")
    public ResponseEntity<String> deleteMyAccount(
            @AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        Long userId = Long.valueOf(userDetails.getUsername());
        userService.deleteUser(userId);
        return ResponseEntity.ok("계정이 삭제되었습니다.");
    }

    @GetMapping("/favorites")
    @Operation(summary = "내 즐겨찾기 레시피 조회", description = "내가 즐겨찾기에 등록한 레시피 목록을 페이지네이션하여 반환합니다. 정렬 기준: createdAt DESC")
    public ResponseEntity<Page<RecipeSimpleDto>> getMyFavorites(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(name = "sort", description = "정렬 기준 (createdAt,DESC 만 지원)", example = "createdAt,DESC")
            @PageableDefault(size = 10, sort = "createdAt", direction = DESC) Pageable pageable) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        Page<RecipeSimpleDto> page = userService.getFavoriteRecipesByUser(userId, userId, pageable);
        return ResponseEntity.ok(page);
    }

    @GetMapping("/recipes")
    @Operation(summary = "내가 작성한 레시피 조회", description = "내가 작성한 모든 레시피를 페이징하여 조회합니다. 정렬 기준: createdAt DESC")
    public ResponseEntity<Page<MyRecipeSummaryDto>> getMyRecipes(
            @AuthenticationPrincipal CustomUserDetails userDetails,
            @Parameter(name = "sort", description = "정렬 기준 (createdAt,DESC 만 지원)", example = "createdAt,DESC")
            @PageableDefault(size = 10, sort = "createdAt", direction = Sort.Direction.DESC)
            Pageable pageable) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        Long myId = userDetails.getUser().getId();
        Page<MyRecipeSummaryDto> page =
                userService.getUserRecipes(myId, myId, pageable);

        return ResponseEntity.ok(page);
    }

    @GetMapping("/streak")
    @Operation(summary = "요리 연속 기록 조회", description = "내 요리 연속 기록 및 최근 요리 일수를 조회합니다.")
    public ResponseEntity<CookingStreakDto> getMyCookingStreak(
            @AuthenticationPrincipal CustomUserDetails userDetails
    ) {
        if (userDetails == null) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        Long userId = userDetails.getUser().getId();
        CookingStreakDto stats = cookingService.getCookingStreakInfo(userId);
        return ResponseEntity.ok(stats);
    }
}
