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
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.oauth2.client.OAuth2AuthorizeRequest;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClientManager;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.ClientRegistrationRepository;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.endpoint.OAuth2ParameterNames;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthService {

    private final ClientRegistrationRepository clients;
    private final OAuth2AuthorizedClientManager authorizedClientManager;
    private final CustomOAuth2UserService customOAuth2UserService;
    private final JwtTokenProvider jwtTokenProvider;
    private final RefreshTokenRepository refreshTokenRepository;
    /**
     * provider: "google", "kakao" 또는 "naver"
     * code: OAuth2 인증 서버가 보낸 인가 코드
     */
    public AuthTokens handleLogin(String provider,String code, String env) {
        log.info("[AuthService] handleLogin start: provider={}, code={}, env={}", provider, code, env);
        OAuth2User oAuth2User = exchangeCodeAndLoadUser(provider, code, env);
        User user = ((CustomOAuth2User) oAuth2User).getUser();
        log.info("[AuthService] OAuth2User loaded: userId={}", user.getId());

        String accessToken  = jwtTokenProvider.createAccessToken(user);
        String refreshToken = jwtTokenProvider.createRefreshToken();
        refreshTokenRepository.save(RefreshToken.builder()
                .user(user)
                .token(refreshToken)
                .build()
        );
        log.info("[AuthService] JWT tokens created");

        return new AuthTokens(accessToken, refreshToken);
    }

    private OAuth2User exchangeCodeAndLoadUser(String registrationId, String code, String env) {
        ClientRegistration registration = clients.findByRegistrationId(registrationId);
        log.info("[AuthService] Found ClientRegistration for {}", registrationId);

        Authentication principal = new AnonymousAuthenticationToken(
                "key", "anonymous", List.of(new SimpleGrantedAuthority("ROLE_ANONYMOUS"))
        );

        String redirectUri;
        if ("local".equalsIgnoreCase(env)) {
            redirectUri = "http://localhost:3000/api/auth/callback/" + registrationId;
        } else {
            redirectUri = "https://www.haemeok.com/api/auth/callback/" + registrationId;
        }
        log.info("[AuthService] Using redirectUri={}", redirectUri);

        OAuth2AuthorizeRequest authRequest = OAuth2AuthorizeRequest.withClientRegistrationId(registrationId)
                .principal(principal)
                .attribute(OAuth2ParameterNames.CODE, code)
                .attribute(OAuth2ParameterNames.REDIRECT_URI, redirectUri)
                .build();
        log.info("[AuthService] Built OAuth2AuthorizeRequest attributes={}", authRequest.getAttributes());

        OAuth2AuthorizedClient client = authorizedClientManager.authorize(authRequest);

        if (client == null) {
            log.error("[AuthService] authorize(...) returned null for {}", registrationId);
            throw new IllegalStateException("Failed to authorize client for registration ID: " + registrationId);
        }
        log.info("[AuthService] AuthorizedClient received: accessToken={}, expiresAt={}",
                client.getAccessToken().getTokenValue().substring(0,10) + "...",
                client.getAccessToken().getExpiresAt());
        OAuth2UserRequest userRequest =
                new OAuth2UserRequest(registration, client.getAccessToken());
        return customOAuth2UserService.loadUser(userRequest);
    }
}