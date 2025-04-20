package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.TokenResponseDTO;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.*;

@RestController
@Profile("local")
@RequiredArgsConstructor
public class LocalAuthController {

    private final JwtTokenProvider jwtTokenProvider;
    private final UserRepository userRepo;

    /**
     * 로컬 테스트용: userId로 액세스 토큰 발급
     * 사용법: GET /local-token?userId=4
     */
    @GetMapping("/local-token")
    public TokenResponseDTO devToken(@RequestParam Long userId) {
        User user = userRepo.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User not found: " + userId));

        String accessToken = jwtTokenProvider.createAccessToken(user);
        return new TokenResponseDTO(accessToken, null);
    }
}
