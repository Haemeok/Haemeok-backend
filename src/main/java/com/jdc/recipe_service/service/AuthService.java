package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.domain.dto.auth.AuthTokens;
import com.jdc.recipe_service.domain.dto.auth.RefreshResult;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.security.oauth.AppleClientSecretGenerator;
import com.jdc.recipe_service.security.oauth.CustomOAuth2User;
import com.jdc.recipe_service.security.oauth.CustomOAuth2UserService;
import io.jsonwebtoken.JwtException;
import io.micrometer.core.instrument.MeterRegistry;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.oauth2.client.endpoint.OAuth2AccessTokenResponseClient;
import org.springframework.security.oauth2.client.endpoint.OAuth2AuthorizationCodeGrantRequest;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.endpoint.OAuth2AccessTokenResponse;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationExchange;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationRequest;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationResponse;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Duration;
import java.time.LocalDateTime;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthService {

    private static final Duration REFRESH_GRACE_WINDOW = Duration.ofSeconds(30);

    private final ClientRegistrationRepository clientRegistrationRepository;
    private final CustomOAuth2UserService customOAuth2UserService;
    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;
    private final OAuth2AccessTokenResponseClient<OAuth2AuthorizationCodeGrantRequest> accessTokenResponseClient;
    private final AppleClientSecretGenerator appleClientSecretGenerator;
    private final MeterRegistry meterRegistry;

    /**
     * refresh 토큰 회전. Android WebView CookieManager flush 누락으로 옛 토큰이 재전송되는
     * 케이스를 살려주기 위해 previous_token grace 윈도우를 두고, 동시 회전을 위해
     * 두 단계 lookup을 모두 PESSIMISTIC_WRITE로 잡는다.
     *
     * <p>호출 전제:
     * <ul>
     *   <li>컨트롤러가 쿠키 존재 여부만 확인하고 원본 token 문자열을 넘긴다.</li>
     *   <li>본 메서드는 @Transactional이므로 두 lookup이 같은 read view에 속하지 않고,
     *       row-level X-lock이 commit 시점까지 유지된다.</li>
     * </ul>
     *
     * <p>실패 매핑:
     * <ul>
     *   <li>JWT 서명/형식 오류 → {@link ErrorCode#INVALID_REFRESH_TOKEN}</li>
     *   <li>JWT 만료 → {@link ErrorCode#REFRESH_TOKEN_EXPIRED}</li>
     *   <li>DB에서 현재/grace 어디에도 없음 → {@link ErrorCode#INVALID_REFRESH_TOKEN}</li>
     *   <li>DB row 만료 → {@link ErrorCode#REFRESH_TOKEN_EXPIRED}</li>
     * </ul>
     */
    @Transactional
    public RefreshResult refresh(String oldToken) {
        validateJwtOrThrow(oldToken);

        LocalDateTime now = LocalDateTime.now();

        var currentRow = refreshTokenRepository.findByTokenForUpdate(oldToken);
        if (currentRow.isPresent()) {
            RefreshToken row = currentRow.get();
            if (isExpired(row, now)) {
                incrementRefresh("expired");
                log.warn("[AUTH_REFRESH] result=fail reason=db_expired stage=current refreshFp={} userId={}",
                        fingerprint(oldToken), row.getUser().getId());
                throw new CustomException(ErrorCode.REFRESH_TOKEN_EXPIRED);
            }
            return rotate(row, oldToken, now);
        }

        var graceRow = refreshTokenRepository.findByPreviousTokenInGraceForUpdate(oldToken, now);
        if (graceRow.isPresent()) {
            RefreshToken row = graceRow.get();
            if (isExpired(row, now)) {
                incrementRefresh("expired");
                log.warn("[AUTH_REFRESH] result=fail reason=db_expired stage=grace refreshFp={} userId={}",
                        fingerprint(oldToken), row.getUser().getId());
                throw new CustomException(ErrorCode.REFRESH_TOKEN_EXPIRED);
            }
            return graceReplay(row);
        }

        boolean graceExpiredReplay = refreshTokenRepository.existsByPreviousToken(oldToken);
        String reason = graceExpiredReplay ? "replay_suspected" : "invalid";
        incrementRefresh(reason);
        log.warn("[AUTH_REFRESH] result=fail reason={} refreshFp={}", reason, fingerprint(oldToken));
        throw new CustomException(ErrorCode.INVALID_REFRESH_TOKEN);
    }

    private void validateJwtOrThrow(String token) {
        try {
            jwtTokenProvider.validateToken(token);
        } catch (JwtException e) {
            boolean expired = e.getMessage() != null && e.getMessage().contains("만료");
            if (expired) {
                incrementRefresh("jwt_expired");
                log.warn("[AUTH_REFRESH] result=fail reason=jwt_expired refreshFp={}", fingerprint(token));
                throw new CustomException(ErrorCode.REFRESH_TOKEN_EXPIRED, e.getMessage());
            }
            incrementRefresh("jwt_invalid");
            log.warn("[AUTH_REFRESH] result=fail reason=jwt_invalid refreshFp={}", fingerprint(token));
            throw new CustomException(ErrorCode.INVALID_REFRESH_TOKEN, e.getMessage());
        }
    }

    private boolean isExpired(RefreshToken row, LocalDateTime now) {
        return row.getExpiredAt() != null && row.getExpiredAt().isBefore(now);
    }

    private RefreshResult rotate(RefreshToken row, String oldToken, LocalDateTime now) {
        User user = row.getUser();

        String newAccess = jwtTokenProvider.createAccessToken(user);
        String newRefresh = jwtTokenProvider.createRefreshToken();
        LocalDateTime newExpiry = jwtTokenProvider.getRefreshTokenExpiryAsLocalDateTime();

        row.setPreviousToken(oldToken);
        row.setPreviousTokenGraceUntil(now.plus(REFRESH_GRACE_WINDOW));
        row.setToken(newRefresh);
        row.setExpiredAt(newExpiry);

        incrementRefresh("rotated");

        return RefreshResult.builder()
                .userId(user.getId())
                .accessToken(newAccess)
                .refreshToken(newRefresh)
                .refreshExpiredAt(newExpiry)
                .path(RefreshResult.Path.ROTATED)
                .build();
    }

    private RefreshResult graceReplay(RefreshToken row) {
        User user = row.getUser();
        String newAccess = jwtTokenProvider.createAccessToken(user);

        incrementRefresh("grace_replay");

        return RefreshResult.builder()
                .userId(user.getId())
                .accessToken(newAccess)
                .refreshToken(row.getToken())
                .refreshExpiredAt(row.getExpiredAt())
                .path(RefreshResult.Path.GRACE_REPLAY)
                .build();
    }

    private void incrementRefresh(String result) {
        meterRegistry.counter("auth_refresh_total", "result", result).increment();
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

    /**
     * provider: "google", "kakao" 또는 "naver"
     * code: OAuth2 인증 서버가 보낸 인가 코드
     */
    public AuthTokens handleLogin(String provider, String code, String env) {
        log.info("[AuthService] handleLogin start: provider={}, code={}, env={}", provider, code, env);

        OAuth2User oAuth2User = exchangeCodeAndLoadUser(provider, code, env);
        User user = ((CustomOAuth2User) oAuth2User).getUser();
        log.info("[AuthService] OAuth2User loaded: userId={}", user.getId());

        String accessToken = jwtTokenProvider.createAccessToken(user);
        String refreshToken = jwtTokenProvider.createRefreshToken();

        LocalDateTime refreshTokenExpiry = jwtTokenProvider.getRefreshTokenExpiryAsLocalDateTime();
        refreshTokenRepository.save(RefreshToken.builder()
                .user(user)
                .token(refreshToken)
                .expiredAt(refreshTokenExpiry)
                .build()
        );
        log.info("[AuthService] JWT tokens created");

        return new AuthTokens(accessToken, refreshToken);
    }

    private OAuth2User exchangeCodeAndLoadUser(String registrationId, String code, String env) {
        ClientRegistration clientRegistration = clientRegistrationRepository.findByRegistrationId(registrationId);
        log.info("[AuthService] Found ClientRegistration for {}", registrationId);

        if ("apple".equals(registrationId)) {
            String appleClientSecret = appleClientSecretGenerator.createClientSecret();
            clientRegistration = ClientRegistration.withClientRegistration(clientRegistration)
                    .clientSecret(appleClientSecret)
                    .build();
        }

        String redirectUri;
        if ("local".equalsIgnoreCase(env)) {
            redirectUri = "http://localhost:3000/api/auth/callback/" + registrationId;
        }
        else if (env != null && (env.startsWith("http://") || env.startsWith("https://"))) {
            String baseUrl = env.endsWith("/") ? env.substring(0, env.length() - 1) : env;
            redirectUri = baseUrl + "/api/auth/callback/" + registrationId;
        }
        else {
            redirectUri = "https://www.recipio.kr/api/auth/callback/" + registrationId;
        }
        log.info("[AuthService] Using redirectUri={}", redirectUri);

        OAuth2AuthorizationRequest authRequest = OAuth2AuthorizationRequest
                .authorizationCode()
                .clientId(clientRegistration.getClientId())
                .authorizationUri(clientRegistration.getProviderDetails().getAuthorizationUri())
                .redirectUri(redirectUri)
                .scopes(clientRegistration.getScopes())
                .state("state-dummy")
                .build();

        OAuth2AuthorizationResponse authResponse = OAuth2AuthorizationResponse
                .success(code)
                .redirectUri(redirectUri)
                .state("state-dummy")
                .build();

        OAuth2AuthorizationCodeGrantRequest grantRequest =
                new OAuth2AuthorizationCodeGrantRequest(clientRegistration, new OAuth2AuthorizationExchange(authRequest, authResponse));

        OAuth2AccessTokenResponse tokenResponse;
        try {
            tokenResponse = accessTokenResponseClient.getTokenResponse(grantRequest);
        } catch (Exception e) {
            log.warn("[AuthService] 토큰 교환 1차 실패, 1초 후 재시도 (provider={}, error={})",
                    registrationId, e.getMessage());
            try {
                Thread.sleep(1000);
                tokenResponse = accessTokenResponseClient.getTokenResponse(grantRequest);
                log.info("[AuthService] 토큰 교환 재시도 성공 (provider={})", registrationId);
            } catch (InterruptedException ie) {
                Thread.currentThread().interrupt();
                throw e;
            } catch (Exception retryEx) {
                log.error("[AuthService] 토큰 교환 최종 실패 (provider={}, redirectUri={}, exceptionType={}, message={})",
                        registrationId, redirectUri, retryEx.getClass().getSimpleName(), retryEx.getMessage());
                throw retryEx;
            }
        }

        try {
            OAuth2UserRequest userRequest = new OAuth2UserRequest(
                    clientRegistration,
                    tokenResponse.getAccessToken(),
                    tokenResponse.getAdditionalParameters()
            );
            return customOAuth2UserService.loadUser(userRequest);
        } catch (Exception e) {
            log.error("[AuthService] 유저 정보 조회/저장 실패 (provider={}, exceptionType={}, message={})",
                    registrationId, e.getClass().getSimpleName(), e.getMessage());
            throw e;
        }
    }
}