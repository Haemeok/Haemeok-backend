package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.user.UserRequestDTO;
import com.jdc.recipe_service.domain.dto.user.UserResponseDTO;
import com.jdc.recipe_service.domain.type.QuotaType;
import com.jdc.recipe_service.service.UserService;
import com.jdc.recipe_service.service.token.TokenService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/admin/users")
@RequiredArgsConstructor
@Tag(name = "관리자 사용자 API", description = "관리자 권한으로 사용자 계정 생성, 조회, 삭제 등을 수행하는 API입니다.")
public class AdminUserController {

    private final UserService userService;
    private final TokenService tokenService;

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "사용자 생성", description = "관리자가 새 사용자 계정을 생성합니다. (소셜 인증은 포함되지 않음)")
    public ResponseEntity<UserResponseDTO> createUser(
            @Valid @RequestBody UserRequestDTO dto) {
        UserResponseDTO response = userService.createUser(dto);
        return new ResponseEntity<>(response, HttpStatus.CREATED);
    }

    @GetMapping
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "전체 사용자 목록 조회", description = "관리자가 모든 사용자 계정 목록을 조회합니다.")
    public ResponseEntity<List<UserResponseDTO>> getAllUsers() {
        List<UserResponseDTO> users = userService.getAllUsers();
        return ResponseEntity.ok(users);
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "특정 사용자 조회", description = "관리자가 특정 사용자 계정 정보를 조회합니다.")
    public ResponseEntity<UserResponseDTO> getUser(@DecodeId Long id) {
        UserResponseDTO response = userService.getUser(id);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    @Operation(summary = "사용자 삭제", description = "관리자가 특정 사용자 계정을 하드 삭제합니다.")
    public ResponseEntity<String> deleteUser(@DecodeId Long id) {
        userService.deleteUser(id);
        return ResponseEntity.ok("사용자가 삭제되었습니다.");
    }

    @PostMapping("/tokens/bulk-give")
    @PreAuthorize("hasAnyRole('ADMIN')")
    public ResponseEntity<String> bulkGiveToken(@RequestBody BulkTokenRequest request) {

        int count = tokenService.giveTokenToUsersBulk(
                request.getUserIds(),
                request.getType(),
                request.getAmount()
        );

        return ResponseEntity.ok(count + "명의 유저에게 " + request.getType() + " 토큰 지급 완료");
    }

    @Data
    public static class BulkTokenRequest {
        private List<Long> userIds;
        private QuotaType type;
        private int amount;
    }
}
