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
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Parameter;

import java.time.LocalDateTime;
import java.util.Optional;


@RestController
@RequestMapping("/api/token")
@RequiredArgsConstructor
@Tag(name = "인증 및 토큰 API", description = "Access/Refresh 토큰 재발급 및 로그아웃 관련 기능을 제공합니다.")
public class AuthController {

    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;

    @PostMapping("/refresh")
    @Operation(
            summary = "Access Token 재발급",
            description = "유효한 Refresh Token 쿠키가 존재할 경우 새로운 Access Token을 발급합니다. Refresh Token도 갱신됩니다."
    )
    public ResponseEntity<?> refreshAccessToken(
            @Parameter(hidden = true)
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
    @Operation(
            summary = "로그아웃",
            description = "현재 사용자의 Access Token을 검증하고, Refresh Token을 삭제하며 쿠키도 무효화합니다."
    )
    public ResponseEntity<Void> logout(
            @Parameter(description = "Access Token 헤더", example = "Bearer {accessToken}")
            @RequestHeader(value = "Authorization", required = false) String authorization,
            @Parameter(hidden = true)
            @CookieValue(value = "refreshToken", required = false) String refreshToken,
            HttpServletResponse response) {

        if (authorization == null || !authorization.startsWith("Bearer ")) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }
        String accessToken = authorization.substring(7);
        if (!jwtTokenProvider.validateToken(accessToken)) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }

        try {
            Optional.ofNullable(refreshToken)
                    .flatMap(refreshTokenRepository::findByToken)
                    .ifPresent(refreshTokenRepository::delete);

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
    @Operation(
            summary = "전체 로그아웃",
            description = "모든 기기에서 사용자의 Refresh Token을 삭제하여 전체 로그아웃을 수행합니다."
    )
    public ResponseEntity<Void> logoutAll(
            @Parameter(description = "Access Token 헤더", example = "Bearer {accessToken}")
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
