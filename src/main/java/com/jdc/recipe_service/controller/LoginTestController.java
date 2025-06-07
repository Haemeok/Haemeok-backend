package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.security.oauth.CustomOAuth2User;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;

@RestController
@RequiredArgsConstructor
@Tag(name = "로그인 테스트", description = "로그인 인증 여부를 확인하는 테스트용 API입니다.")
public class LoginTestController {

    @Operation(summary = "보호된 리소스 접근 테스트", description = "인증된 사용자만 접근 가능한 API입니다. 인증된 사용자의 정보를 반환합니다.")
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
}
