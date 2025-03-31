package com.jdc.recipe_service.security.oauth;

import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.Role;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    private final UserRepository userRepository;

    @Override
    public OAuth2User loadUser(OAuth2UserRequest userRequest) {
        OAuth2User oAuth2User = super.loadUser(userRequest);

        String provider = userRequest.getClientRegistration().getRegistrationId(); // google, kakao, naver
        String oauthId = oAuth2User.getName(); // 각 플랫폼 고유 식별자

        // 사용자 이메일 or 프로필 등 정보 꺼내기 (단순화된 예시)
        String nickname = (String) oAuth2User.getAttributes().get("name");
        String profileImage = (String) oAuth2User.getAttributes().get("picture");

        // 기존 사용자 조회 or 새로 생성
        User user = userRepository.findByProviderAndOauthId(provider, oauthId)
                .orElseGet(() -> userRepository.save(User.builder()
                        .provider(provider)
                        .oauthId(oauthId)
                        .nickname(nickname)
                        .role(Role.USER)
                        .profileImage(profileImage)
                        .build()));

        return new CustomOAuth2User(user, oAuth2User.getAttributes());
    }
}