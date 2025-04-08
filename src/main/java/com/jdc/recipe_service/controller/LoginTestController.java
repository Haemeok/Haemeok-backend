package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.user.UserResponseDTO;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.security.oauth.CustomOAuth2User;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
public class LoginTestController {

    @GetMapping("/api/protected")
    public String protectedResource(Authentication authentication) {
        if (authentication == null || !(authentication.getPrincipal() instanceof CustomOAuth2User)) {
            return "인증된 사용자 정보가 없습니다.";
        }

        CustomOAuth2User user = (CustomOAuth2User) authentication.getPrincipal();

        return String.format(
                "로그인 인증 완료!\n\n유저 정보:\n- ID: %d\n- 닉네임: %s\n- 소셜: %s",
                user.getUser().getId(),
                user.getUser().getNickname(),
                user.getUser().getProvider()
        );
    }
    @GetMapping("/api/me")
    public ResponseEntity<UserResponseDTO> getMyInfo(@AuthenticationPrincipal CustomUserDetails userDetails) {
        if (userDetails == null) {
            return ResponseEntity.status(401).build(); // 인증 안 됐을 경우
        }

        User user = userDetails.getUser();
        UserResponseDTO response = new UserResponseDTO(user);
        return ResponseEntity.ok(response);
    }
}
