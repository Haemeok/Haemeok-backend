package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.TokenResponseDTO;
import com.jdc.recipe_service.domain.dto.auth.RefreshResult;
import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.exception.ErrorResponse;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.service.AuthService;
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
import java.time.Duration;
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
    private final AuthService authService;

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

        RefreshResult result = authService.refresh(refreshToken);

        log.info("[AUTH_REFRESH] result=success path={} userId={} oldRefreshFp={} newRefreshFp={} refreshExpiredAt={} origin={} userAgent={}",
                result.getPath(), result.getUserId(), refreshFp,
                fingerprint(result.getRefreshToken()), result.getRefreshExpiredAt(), origin, userAgent);

        writeAuthCookies(response, result, request);

        return ResponseEntity.ok(new TokenResponseDTO(result.getAccessToken(), null));
    }

    // refresh/access 쿠키를 한 세트로 내려준다. refresh maxAge는 DB가 들고 있는 실제 만료 시각
    // 기준으로 계산해 서버/클라이언트 TTL이 어긋나지 않게 한다. grace_replay 경로에서도 기존
    // refresh가 그대로 재전송되므로 max-age 역시 기존 만료 기준으로 음수가 아닌 남은 시간을 준다.
    private void writeAuthCookies(HttpServletResponse response, RefreshResult result, HttpServletRequest request) {
        String origin = request.getHeader("Origin");
        boolean isLocalRequest = origin != null && origin.startsWith("http://localhost");

        long refreshMaxAgeSeconds = Math.max(
                Duration.between(LocalDateTime.now(), result.getRefreshExpiredAt()).getSeconds(),
                0L
        );

        var refreshBuilder = ResponseCookie.from("refreshToken", result.getRefreshToken())
                .path("/")
                .httpOnly(true)
                .maxAge(refreshMaxAgeSeconds)
                .sameSite("Lax");
        var accessBuilder = ResponseCookie.from("accessToken", result.getAccessToken())
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
            // 클라이언트가 회전 직전의 옛 토큰을 들고 logout을 눌러도(Android WebView flush 지연 등)
            // 서버의 current row가 살아 남아 "로그아웃했는데 세션이 살아있는" 상태가 되지 않도록,
            // token/previous_token 양쪽을 같이 매칭해서 revoke한다.
            var rows = refreshTokenRepository.findAllByTokenOrPreviousToken(refreshToken);
            if (!rows.isEmpty()) {
                refreshTokenRepository.deleteAll(rows);
            }
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
