package com.jdc.recipe_service.security.oauth;

import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.Role;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.RandomStringUtils;
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

        String provider     = userRequest.getClientRegistration().getRegistrationId();
        String oauthId      = oAuth2User.getName();
        String baseNickname = (String) oAuth2User.getAttributes().get("name");
        String profileImage = (String) oAuth2User.getAttributes().get("picture");

        User user = userRepository.findByProviderAndOauthId(provider, oauthId)
                .orElseGet(() -> {
                    // 닉네임 중복 검사 & 랜덤값 추가
                    String nickname = makeUniqueNickname(baseNickname);
                    return userRepository.save(User.builder()
                            .provider(provider)
                            .oauthId(oauthId)
                            .nickname(nickname)
                            .role(Role.USER)
                            .profileImage(profileImage)
                            .build());
                });

        return new CustomOAuth2User(user, oAuth2User.getAttributes());
    }


    private String makeUniqueNickname(String baseNickname) {
        String candidate = baseNickname;
        // DB에 존재하면 랜덤 suffix
        while (userRepository.existsByNickname(candidate)) {
            String suffix = RandomStringUtils.randomAlphanumeric(5);
            candidate = baseNickname + "_" + suffix;
        }
        return candidate;
    }
}
