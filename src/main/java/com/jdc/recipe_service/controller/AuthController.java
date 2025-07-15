package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.TokenResponseDTO;
import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.core.env.Environment;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Parameter;

import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.Optional;


@RestController
@RequestMapping("/api/token")
@RequiredArgsConstructor
@Tag(name = "인증 및 토큰 API", description = "Access/Refresh 토큰 재발급 및 로그아웃 관련 기능을 제공합니다.")
public class AuthController {

    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;
    private final Environment env;

    private boolean isLocal() {
        return Arrays.asList(env.getActiveProfiles()).contains("local");
    }

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

        var refreshBuilder = ResponseCookie.from("refreshToken", newRefreshToken)
                .path("/")
                .httpOnly(true)
                .secure(true)
                .maxAge(7 * 24 * 60 * 60)
                .sameSite("Lax");
        var accessBuilder  = ResponseCookie.from("accessToken", newAccessToken)
                .path("/")
                .httpOnly(true)
                .secure(true)
                .maxAge(15 * 60)
                .sameSite("Lax");

        if (!isLocal()) {
            refreshBuilder.domain(".haemeok.com");
            accessBuilder .domain(".haemeok.com");
        }

        response.addHeader(HttpHeaders.SET_COOKIE, refreshBuilder.build().toString());
        response.addHeader(HttpHeaders.SET_COOKIE, accessBuilder .build().toString());

        return ResponseEntity.ok(new TokenResponseDTO(newAccessToken, null));
    }

    @PostMapping("/logout")
    @Operation(
            summary = "로그아웃",
            description = "현재 사용자의 Access Token을 검증하고, 쿠키도 무효화합니다."
    )
    public ResponseEntity<Void> logout(
            @Parameter(hidden = true)
            @CookieValue(value = "accessToken",  required = false) String accessToken,
            @CookieValue(value = "refreshToken", required = false) String refreshToken,
            HttpServletResponse response) {

        if (accessToken == null || !jwtTokenProvider.validateToken(accessToken)) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }

        Optional.ofNullable(refreshToken)
                .flatMap(refreshTokenRepository::findByToken)
                .ifPresent(refreshTokenRepository::delete);

        var deleteRefresh = ResponseCookie.from("refreshToken", "")
                .path("/")
                .httpOnly(true)
                .secure(true)
                .maxAge(0)
                .sameSite("Lax");
        var deleteAccess  = ResponseCookie.from("accessToken", "")
                .path("/")
                .httpOnly(true)
                .secure(true)
                .maxAge(0)
                .sameSite("Lax");

        if (!isLocal()) {
            deleteRefresh.domain(".haemeok.com");
            deleteAccess .domain(".haemeok.com");
        }

        response.addHeader(HttpHeaders.SET_COOKIE, deleteRefresh.build().toString());
        response.addHeader(HttpHeaders.SET_COOKIE, deleteAccess .build().toString());

        return ResponseEntity.ok().build();
    }

    @PostMapping("/logout/all")
    @Transactional
    @Operation(
            summary = "전체 로그아웃",
            description = "모든 기기에서 사용자의 Refresh Token을 삭제하여 전체 로그아웃을 수행합니다."
    )
    public ResponseEntity<Void> logoutAll(
            @Parameter(hidden = true)
            @CookieValue(value = "accessToken", required = false) String accessToken,
            HttpServletResponse response) {

        if (accessToken == null || !jwtTokenProvider.validateToken(accessToken)) {
            throw new CustomException(ErrorCode.AUTH_UNAUTHORIZED);
        }

        Long userId = jwtTokenProvider.getUserIdFromToken(accessToken);
        refreshTokenRepository.deleteByUserId(userId);

        var deleteRefresh = ResponseCookie.from("refreshToken", "")
                .path("/")
                .httpOnly(true)
                .secure(true)
                .maxAge(0)
                .sameSite("Lax");
        var deleteAccess  = ResponseCookie.from("accessToken", "")
                .path("/")
                .httpOnly(true)
                .secure(true)
                .maxAge(0)
                .sameSite("Lax");

        if (!isLocal()) {
            deleteRefresh.domain(".haemeok.com");
            deleteAccess .domain(".haemeok.com");
        }

        response.addHeader(HttpHeaders.SET_COOKIE, deleteRefresh.build().toString());
        response.addHeader(HttpHeaders.SET_COOKIE, deleteAccess .build().toString());

        return ResponseEntity.ok().build();
    }
}
