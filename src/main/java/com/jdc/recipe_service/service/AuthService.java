package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.domain.dto.auth.AuthTokens;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.security.oauth.CustomOAuth2User;
import com.jdc.recipe_service.security.oauth.CustomOAuth2UserService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.oauth2.client.endpoint.DefaultAuthorizationCodeTokenResponseClient;
import org.springframework.security.oauth2.client.endpoint.OAuth2AccessTokenResponseClient;
import org.springframework.security.oauth2.client.endpoint.OAuth2AuthorizationCodeGrantRequest;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.OAuth2AccessToken;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationExchange;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationRequest;
import org.springframework.security.oauth2.core.endpoint.OAuth2AuthorizationResponse;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;

import java.time.LocalDateTime;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthService {

    private final ClientRegistrationRepository clientRegistrationRepository;
    private final CustomOAuth2UserService customOAuth2UserService;
    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;
    private final OAuth2AccessTokenResponseClient<OAuth2AuthorizationCodeGrantRequest> accessTokenResponseClient;

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

        String redirectUri;
        if ("local".equalsIgnoreCase(env)) {
            redirectUri = "http://localhost:3000/api/auth/callback/" + registrationId;
        } else {
            redirectUri = "https://www.haemeok.com/api/auth/callback/" + registrationId;
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

        log.info("[AuthService] Requesting access token directly...");
        OAuth2AccessToken accessToken = accessTokenResponseClient.getTokenResponse(grantRequest).getAccessToken();
        log.info("[AuthService] Access token received successfully.");

        OAuth2UserRequest userRequest = new OAuth2UserRequest(clientRegistration, accessToken);
        return customOAuth2UserService.loadUser(userRequest);
    }
}