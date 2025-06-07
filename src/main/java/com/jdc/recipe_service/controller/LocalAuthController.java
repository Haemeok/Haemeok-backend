package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.TokenResponseDTO;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "로컬 인증 테스트", description = "로컬 개발 환경에서 사용자 ID로 액세스 토큰을 발급합니다.")
@RestController
@Profile("local")
@RequiredArgsConstructor
public class LocalAuthController {

    private final JwtTokenProvider jwtTokenProvider;
    private final UserRepository userRepo;

    @Operation(summary = "로컬 테스트용 액세스 토큰 발급", description = "지정한 userId로 JWT 액세스 토큰을 발급합니다.")
    @GetMapping("/local-token")
    public TokenResponseDTO devToken(
            @Parameter(description = "사용자 ID", example = "4") @RequestParam Long userId) {

        User user = userRepo.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found: " + userId));

        String accessToken = jwtTokenProvider.createAccessToken(user);
        return new TokenResponseDTO(accessToken, null);
    }
}
