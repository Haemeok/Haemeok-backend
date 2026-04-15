package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.TokenResponseDTO;
import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.exception.ErrorResponse;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import io.jsonwebtoken.JwtException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseCookie;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Parameter;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.LocalDateTime;

@RestController
@RequestMapping("/api/token")
@RequiredArgsConstructor
@Tag(name = "인증 및 토큰 API", description = "Access/Refresh 토큰 재발급 및 로그아웃 관련 기능을 제공합니다.")
@Slf4j
public class AuthController {

    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;
    private final UserRepository userRepository;

    @PostMapping("/refresh")
    @Operation(
            summary = "Access Token 재발급",
            description = "유효한 Refresh Token 쿠키가 존재할 경우 새로운 Access Token을 발급합니다. Refresh Token도 갱신됩니다."
    )
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "토큰 재발급 성공",
                    content = @Content(schema = @Schema(implementation = TokenResponseDTO.class))),
            @ApiResponse(responseCode = "400", description = "리프레시 토큰 없음 또는 유효하지 않음 (code: 601) / 리프레시 토큰 만료 (code: 602)",
                    content = @Content(schema = @Schema(implementation = ErrorResponse.class)))
    })
    public ResponseEntity<?> refreshAccessToken(
            @Parameter(hidden = true)
            @CookieValue(name = "refreshToken", required = false) String refreshToken,
            HttpServletRequest request,
            HttpServletResponse response
    ) {
        String origin = request.getHeader("Origin");
        String userAgent = request.getHeader("User-Agent");
        String refreshFp = fingerprint(refreshToken);

        log.info("[AUTH_REFRESH] result=start hasRefreshCookie={} refreshFp={} origin={} userAgent={}",
                refreshToken != null, refreshFp, origin, userAgent);

        if (refreshToken == null || refreshToken.isBlank()) {
            log.warn("[AUTH_REFRESH] result=fail reason=missing_cookie refreshFp={} origin={} userAgent={}",
                    refreshFp, origin, userAgent);
            throw new CustomException(ErrorCode.INVALID_REFRESH_TOKEN);
        }

        try {
            jwtTokenProvider.validateToken(refreshToken);
        } catch (JwtException e) {
            boolean expired = e.getMessage() != null && e.getMessage().contains("만료");
            ErrorCode errorCode = expired ? ErrorCode.REFRESH_TOKEN_EXPIRED : ErrorCode.INVALID_REFRESH_TOKEN;
            String reason = expired ? "jwt_expired" : "jwt_invalid";

            log.warn("[AUTH_REFRESH] result=fail reason={} refreshFp={} origin={} userAgent={} message={}",
                    reason, refreshFp, origin, userAgent, e.getMessage());
            throw new CustomException(errorCode, e.getMessage());
        }

        RefreshToken savedToken = refreshTokenRepository.findByToken(refreshToken)
                .orElseThrow(() -> {
                    log.warn("[AUTH_REFRESH] result=fail reason=db_not_found refreshFp={} origin={} userAgent={}",
                            refreshFp, origin, userAgent);
                    return new CustomException(ErrorCode.INVALID_REFRESH_TOKEN);
                });

        if (savedToken.getExpiredAt().isBefore(LocalDateTime.now())) {
            log.warn("[AUTH_REFRESH] result=fail reason=db_expired userId={} refreshFp={} dbExpiredAt={} origin={} userAgent={}",
                    savedToken.getUser().getId(), refreshFp, savedToken.getExpiredAt(), origin, userAgent);
            throw new CustomException(ErrorCode.REFRESH_TOKEN_EXPIRED);
        }

        User user = savedToken.getUser();
        String newAccessToken = jwtTokenProvider.createAccessToken(user);
        String newRefreshToken = jwtTokenProvider.createRefreshToken();
        LocalDateTime newRefreshExpiry = LocalDateTime.now().plusDays(7);

        savedToken.setToken(newRefreshToken);
        savedToken.setExpiredAt(newRefreshExpiry);
        refreshTokenRepository.save(savedToken);

        boolean isLocalRequest = origin != null && origin.startsWith("http://localhost");

        log.info("[AUTH_REFRESH] result=success userId={} oldRefreshFp={} newRefreshFp={} newExpiredAt={} origin={} userAgent={}",
                user.getId(), refreshFp, fingerprint(newRefreshToken), newRefreshExpiry, origin, userAgent);


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
    @Operation(summary = "로그아웃", description = "현재 기기의 Refresh Token을 삭제하고 쿠키를 제거합니다.")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "로그아웃 성공")
    })
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
    @Operation(summary = "전체 로그아웃", description = "해당 사용자의 모든 기기에서 Refresh Token을 삭제하고 쿠키를 제거합니다.")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "전체 로그아웃 성공")
    })
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

    private String fingerprint(String token) {
        if (token == null || token.isBlank()) {
            return "none";
        }

        try {
            byte[] digest = MessageDigest.getInstance("SHA-256")
                    .digest(token.getBytes(StandardCharsets.UTF_8));
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < 4; i++) {
                sb.append(String.format("%02x", digest[i]));
            }
            return sb.toString();
        } catch (NoSuchAlgorithmException e) {
            return Integer.toHexString(token.hashCode());
        }
    }
}
