package com.jdc.recipe_service.security.oauth;

import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;

@Slf4j
@Component
@RequiredArgsConstructor
public class OAuth2AuthenticationSuccessHandler implements AuthenticationSuccessHandler {

    private static final int MAX_REFRESH_TOKENS = 4;

    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;

    @Override
    @Transactional
    public void onAuthenticationSuccess(HttpServletRequest request,
                                        HttpServletResponse response,
                                        Authentication authentication) throws IOException {
        CustomOAuth2User oAuth2User = (CustomOAuth2User) authentication.getPrincipal();

        // 토큰 생성 & 저장
        String accessToken  = jwtTokenProvider.createAccessToken(oAuth2User.getUser());
        String refreshToken = jwtTokenProvider.createRefreshToken();
        refreshTokenRepository.save(RefreshToken.builder()
                .user(oAuth2User.getUser())
                .token(refreshToken)
                .expiredAt(LocalDateTime.now().plusDays(7))
                .build());

        // 사용자별 토큰 개수 체크 & 초과분 삭제
        List<RefreshToken> tokens = refreshTokenRepository
                .findByUserOrderByCreatedAtAsc(oAuth2User.getUser());

        if (tokens.size() > MAX_REFRESH_TOKENS) {
            int overflow = tokens.size() - MAX_REFRESH_TOKENS;
            for (int i = 0; i < overflow; i++) {
                refreshTokenRepository.delete(tokens.get(i));
            }
        }

        // 리프레시 토큰을 HttpOnly 쿠키에 저장
        Cookie cookie = new Cookie("refreshToken", refreshToken);
        cookie.setHttpOnly(true); // JS 접근 금지
        //cookie.setSecure(true);   // HTTPS에서만 전송됨
        cookie.setPath("/");      // 모든 경로에서 접근 가능
        cookie.setMaxAge(7 * 24 * 60 * 60); // 7일 유효
        response.addCookie(cookie);

        // accessToken만 리다이렉트 URI에 포함
        String referer = request.getHeader("Referer");

        String redirectBase;
        if (referer != null && referer.contains("localhost")) {
            redirectBase = "http://localhost:5173";
        } else {
            redirectBase = "https://www.haemeok.com";
        }

        // accessToken만 전달
        String redirectUri = redirectBase + "/oauth2/redirect" +
                "?accessToken=" + accessToken;

        response.sendRedirect(redirectUri);
    }
}
