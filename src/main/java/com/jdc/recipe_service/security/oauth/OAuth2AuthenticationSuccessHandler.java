package com.jdc.recipe_service.security.oauth;

import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.security.core.Authentication;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.net.URI;
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

        String accessToken  = jwtTokenProvider.createAccessToken(oAuth2User.getUser());
        String refreshToken = jwtTokenProvider.createRefreshToken();
        refreshTokenRepository.save(RefreshToken.builder()
                .user(oAuth2User.getUser())
                .token(refreshToken)
                .expiredAt(LocalDateTime.now().plusDays(7))
                .build());

        List<RefreshToken> tokens = refreshTokenRepository
                .findByUserOrderByCreatedAtAsc(oAuth2User.getUser());
        if (tokens.size() > MAX_REFRESH_TOKENS) {
            int overflow = tokens.size() - MAX_REFRESH_TOKENS;
            for (int i = 0; i < overflow; i++) {
                refreshTokenRepository.delete(tokens.get(i));
            }
        }

        ResponseCookie refreshCookie = ResponseCookie.from("refreshToken", refreshToken)
                .httpOnly(true)
                .secure(true)
                .path("/")
                .maxAge(7 * 24 * 60 * 60)
                .sameSite("None")
                .build();

        ResponseCookie accessCookie = ResponseCookie.from("accessToken", accessToken)
                .httpOnly(true)
                .secure(true)
                .path("/")
                .maxAge(15 * 60)
                .sameSite("None")
                .build();

        response.addHeader(HttpHeaders.SET_COOKIE, refreshCookie.toString());
        response.addHeader(HttpHeaders.SET_COOKIE, accessCookie.toString());

        String referer = request.getHeader("Referer");

        String redirectBase;
        if (referer != null && referer.startsWith("http://localhost")) {
            URI uri = URI.create(referer);
            redirectBase = uri.getScheme() + "://" + uri.getHost();
            if (uri.getPort() != -1) {
                redirectBase += ":" + uri.getPort();
            }
        } else {
            redirectBase = "https://www.haemeok.com";
        }

        response.sendRedirect(redirectBase + "/oauth2/redirect");
    }
}
