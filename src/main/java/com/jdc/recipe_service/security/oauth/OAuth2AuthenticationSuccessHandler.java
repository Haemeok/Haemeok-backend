package com.jdc.recipe_service.security.oauth;

import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.time.LocalDateTime;

@Slf4j
@Component
@RequiredArgsConstructor
public class OAuth2AuthenticationSuccessHandler implements AuthenticationSuccessHandler {

    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;

    @Override
    public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws IOException {
        CustomOAuth2User oAuth2User = (CustomOAuth2User) authentication.getPrincipal();

        // JWT 발급
        String accessToken = jwtTokenProvider.createAccessToken(oAuth2User.getUser());
        String refreshToken = jwtTokenProvider.createRefreshToken();

        // 리프레시 토큰 저장
        refreshTokenRepository.save(
                RefreshToken.builder()
                        .user(oAuth2User.getUser())
                        .token(refreshToken)
                        .expiredAt(LocalDateTime.now().plusDays(7))
                        .build()
        );

        String origin = request.getHeader("Origin");

        // fallback 기본 redirect URI
        String redirectBase = (origin != null) ? origin : "https://www.haemeok.com";

        String redirectUri = redirectBase + "/oauth2/redirect" +
                "?accessToken=" + accessToken +
                "&refreshToken=" + refreshToken;

        response.sendRedirect(redirectUri);
    }
}