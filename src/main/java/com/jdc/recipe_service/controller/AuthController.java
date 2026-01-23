package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.TokenResponseDTO;
import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Parameter;

import java.time.LocalDateTime;

@RestController
@RequestMapping("/api/token")
@RequiredArgsConstructor
@Tag(name = "인증 및 토큰 API", description = "Access/Refresh 토큰 재발급 및 로그아웃 관련 기능을 제공합니다.")
public class AuthController {

    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;
    private final UserRepository userRepository;

    @PostMapping("/refresh")
    @Operation(
            summary = "Access Token 재발급",
            description = "유효한 Refresh Token 쿠키가 존재할 경우 새로운 Access Token을 발급합니다. Refresh Token도 갱신됩니다."
    )
    public ResponseEntity<?> refreshAccessToken(
            @Parameter(hidden = true)
            @CookieValue(name = "refreshToken", required = false) String refreshToken,
            HttpServletRequest request,
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

        String origin = request.getHeader("Origin");
        boolean isLocalRequest = origin != null && origin.startsWith("http://localhost");


        var refreshBuilder = ResponseCookie.from("refreshToken", newRefreshToken)
                .path("/")
                .httpOnly(true)
                .maxAge(7 * 24 * 60 * 60)
                .sameSite("Lax");
        var accessBuilder  = ResponseCookie.from("accessToken", newAccessToken)
                .path("/")
                .httpOnly(true)
                .maxAge(15 * 60)
                .sameSite("Lax");

        if (!isLocalRequest) {
            refreshBuilder.secure(true).domain(".recipio.kr");
            accessBuilder.secure(true).domain(".recipio.kr");
        }

        response.addHeader(HttpHeaders.SET_COOKIE, refreshBuilder.build().toString());
        response.addHeader(HttpHeaders.SET_COOKIE, accessBuilder .build().toString());

        return ResponseEntity.ok(new TokenResponseDTO(newAccessToken, null));
    }

    @PostMapping("/logout")
    @Operation(summary = "로그아웃")
    public ResponseEntity<Void> logout(
            @CookieValue(value = "refreshToken", required = false) String refreshToken,
            HttpServletRequest request,
            HttpServletResponse response) {

        if (refreshToken != null) {
            refreshTokenRepository.findByToken(refreshToken)
                    .ifPresent(refreshTokenRepository::delete);
        }

        ResponseCookie deleteRefresh = createDeleteCookie("refreshToken", request);
        ResponseCookie deleteAccess  = createDeleteCookie("accessToken", request);

        response.addHeader(HttpHeaders.SET_COOKIE, deleteRefresh.toString());
        response.addHeader(HttpHeaders.SET_COOKIE, deleteAccess.toString());

        return ResponseEntity.ok().build();
    }

    @PostMapping("/logout/all")
    @Transactional
    @Operation(summary = "전체 로그아웃")
    public ResponseEntity<Void> logoutAll(
            @Parameter(hidden = true)
            @CookieValue(value = "accessToken", required = false) String accessToken,
            HttpServletRequest request,
            HttpServletResponse response) {

        if (accessToken != null && jwtTokenProvider.validateToken(accessToken)) {
            Long userId = jwtTokenProvider.getUserIdFromToken(accessToken);
            refreshTokenRepository.deleteByUserId(userId);
        }

        ResponseCookie deleteRefresh = createDeleteCookie("refreshToken", request);
        ResponseCookie deleteAccess  = createDeleteCookie("accessToken", request);

        response.addHeader(HttpHeaders.SET_COOKIE, deleteRefresh.toString());
        response.addHeader(HttpHeaders.SET_COOKIE, deleteAccess.toString());

        return ResponseEntity.ok().build();
    }

    @Value("${app.test-login.enabled:false}")
    private boolean isTestLoginEnabled;

    @PostMapping("/test-login")
    @Operation(summary = "애플 심사용 테스트 로그인", description = "특정 계정으로 즉시 로그인하여 토큰을 발급합니다.")
    public ResponseEntity<?> testLogin(HttpServletRequest request, HttpServletResponse response) {
        if (!isTestLoginEnabled) {
            return ResponseEntity.notFound().build();
        }

        User testUser = userRepository.findByProviderAndOauthId("test", "apple_reviewer")
                .orElseThrow(() -> new CustomException(ErrorCode.USER_NOT_FOUND));

        String accessToken = jwtTokenProvider.createAccessToken(testUser);
        String refreshToken = jwtTokenProvider.createRefreshToken();

        RefreshToken savedToken = refreshTokenRepository.findByUser(testUser)
                .orElseGet(() -> RefreshToken.builder()
                        .user(testUser)
                        .build());

        savedToken.setUser(testUser);
        savedToken.setToken(refreshToken);
        savedToken.setExpiredAt(LocalDateTime.now().plusDays(7));
        refreshTokenRepository.save(savedToken);

        String origin = request.getHeader("Origin");
        boolean isLocalRequest = origin != null && origin.startsWith("http://localhost");

        var refreshBuilder = ResponseCookie.from("refreshToken", refreshToken)
                .path("/")
                .httpOnly(true)
                .maxAge(7 * 24 * 60 * 60)
                .sameSite("Lax");
        var accessBuilder  = ResponseCookie.from("accessToken", accessToken)
                .path("/")
                .httpOnly(true)
                .maxAge(15 * 60)
                .sameSite("Lax");

        if (!isLocalRequest) {
            refreshBuilder.secure(true).domain(".recipio.kr");
            accessBuilder.secure(true).domain(".recipio.kr");
        }

        response.addHeader(HttpHeaders.SET_COOKIE, refreshBuilder.build().toString());
        response.addHeader(HttpHeaders.SET_COOKIE, accessBuilder.build().toString());

        return ResponseEntity.ok(new TokenResponseDTO(accessToken, null));
    }

    private ResponseCookie createCookie(String name, String value, long maxAge, HttpServletRequest request) {
        String origin = request.getHeader("Origin");

        ResponseCookie.ResponseCookieBuilder builder = ResponseCookie.from(name, value)
                .path("/")
                .httpOnly(true)
                .maxAge(maxAge);

        if (origin == null || origin.isBlank()) {
            builder.secure(true).sameSite("Lax");
        }
        else if (origin.startsWith("http://localhost") || origin.startsWith("http://127.0.0.1")) {
            builder.secure(false).sameSite("Lax");
        }
        else if (origin.contains("recipio.kr")) {
            builder.secure(true).domain(".recipio.kr").sameSite("Lax");
        }
        else {
            builder.secure(true).sameSite("None");
        }

        return builder.build();
    }

    private ResponseCookie createDeleteCookie(String name, HttpServletRequest request) {
        return createCookie(name, "", 0, request);
    }
}
