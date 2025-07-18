package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.RefreshToken;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RefreshTokenRepository;
import com.jdc.recipe_service.domain.dto.auth.AuthTokens;
import com.jdc.recipe_service.jwt.JwtTokenProvider;
import com.jdc.recipe_service.security.oauth.CustomOAuth2User;
import com.jdc.recipe_service.security.oauth.CustomOAuth2UserService;
import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
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
    public AuthTokens handleLogin(String provider,String code) {
        OAuth2User oAuth2User = exchangeCodeAndLoadUser(provider, code);
        User user = ((CustomOAuth2User) oAuth2User).getUser();

        String accessToken  = jwtTokenProvider.createAccessToken(user);
        String refreshToken = jwtTokenProvider.createRefreshToken();
        refreshTokenRepository.save(RefreshToken.builder()
                .user(user)
                .token(refreshToken)
                .build()
        );

        return new AuthTokens(accessToken, refreshToken);
    }

    private OAuth2User exchangeCodeAndLoadUser(String registrationId, String code) {
        ClientRegistration registration = clients.findByRegistrationId(registrationId);

        Authentication principal =
                new UsernamePasswordAuthenticationToken(registrationId, null, List.of());

        OAuth2AuthorizeRequest authRequest = OAuth2AuthorizeRequest.withClientRegistrationId(registrationId)
                .principal(principal)
                .attribute(OAuth2ParameterNames.CODE, code)
                .build();

        OAuth2AuthorizedClient client = authorizedClientManager.authorize(authRequest);

        OAuth2UserRequest userRequest =
                new OAuth2UserRequest(registration, client.getAccessToken());
        return customOAuth2UserService.loadUser(userRequest);
    }
}