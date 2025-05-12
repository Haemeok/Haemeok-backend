package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.TokenResponseDTO;
import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.Optional;

@RestController
@RequestMapping("/api/token")
@RequiredArgsConstructor
public class AuthController {

    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;

    @PostMapping("/refresh")
    public ResponseEntity<?> refreshAccessToken(
            @CookieValue(name = "refreshToken", required = false) String refreshToken,
            HttpServletResponse response
    ) {
        if (refreshToken == null || !jwtTokenProvider.validateToken(refreshToken)) {
            throw new CustomException(ErrorCode.INVALID_REFRESH_TOKEN);
        }

        RefreshToken savedToken = refreshTokenRepository.findByToken(refreshToken)
                .orElseThrow(() -> new CustomException(ErrorCode.INVALID_REFRESH_TOKEN));

        if (savedToken == null || savedToken.getExpiredAt().isBefore(LocalDateTime.now())) {
            throw new CustomException(ErrorCode.REFRESH_TOKEN_EXPIRED);
        }

        User user = savedToken.getUser();
        String newAccessToken = jwtTokenProvider.createAccessToken(user);
        String newRefreshToken = jwtTokenProvider.createRefreshToken();

        // 기존 토큰 갱신
        savedToken.setToken(newRefreshToken);
        savedToken.setExpiredAt(LocalDateTime.now().plusDays(7));
        refreshTokenRepository.save(savedToken);

        ResponseCookie cookie = ResponseCookie.from("refreshToken", newRefreshToken)
                .httpOnly(true)
                .secure(true)
                .path("/")
                .maxAge(7 * 24 * 60 * 60)
                .sameSite("None")
                .build();
        response.addHeader(HttpHeaders.SET_COOKIE, cookie.toString());

        return ResponseEntity.ok(new TokenResponseDTO(newAccessToken, null));
    }

    @PostMapping("/logout")
    public ResponseEntity<Void> logout(
            @RequestHeader(value = "Authorization", required = false) String authorization,
            @CookieValue(value = "refreshToken", required = false) String refreshToken,
            HttpServletResponse response) {

        // Authorization 헤더 유효성 검사
        if (authorization == null || !authorization.startsWith("Bearer ")) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        String accessToken = authorization.substring(7);
        if (!jwtTokenProvider.validateToken(accessToken)) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }

        try {
            // DB에 등록된 refreshToken 삭제 (없어도 무시)
            Optional.ofNullable(refreshToken)
                    .flatMap(refreshTokenRepository::findByToken)
                    .ifPresent(refreshTokenRepository::delete);

            // 클라이언트 쿠키 삭제
            Cookie deleteCookie = new Cookie("refreshToken", null);
            deleteCookie.setHttpOnly(true);
            deleteCookie.setSecure(true);
            deleteCookie.setPath("/");
            deleteCookie.setMaxAge(0);
            response.addCookie(deleteCookie);

            return ResponseEntity.ok().build();

        } catch (Exception e) {
            throw new CustomException(ErrorCode.LOGOUT_FAILED);
        }
    }

    @PostMapping("/logout/all")
    @Transactional
    public ResponseEntity<Void> logoutAll(
            @RequestHeader(value = "Authorization", required = false) String authorization,
            HttpServletResponse response) {

        if (authorization == null || !authorization.startsWith("Bearer ")) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        String accessToken = authorization.substring(7);
        if (!jwtTokenProvider.validateToken(accessToken)) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }

        Long userId = jwtTokenProvider.getUserIdFromToken(accessToken);

        refreshTokenRepository.deleteByUserId(userId);

        Cookie deleteCookie = new Cookie("refreshToken", null);
        deleteCookie.setHttpOnly(true);
        deleteCookie.setSecure(true);
        deleteCookie.setPath("/");
        deleteCookie.setMaxAge(0);
        response.addCookie(deleteCookie);

        return ResponseEntity.ok().build();
    }
}
