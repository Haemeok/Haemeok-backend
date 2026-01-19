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

import java.security.SecureRandom;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

@Slf4j
@Service
@RequiredArgsConstructor
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    private final UserRepository userRepository;
    private static final String DEFAULT_PROFILE_BASE =
            "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/profiles/default/";
    private static final String ALPHANUM = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
    private static final SecureRandom SECURE_RANDOM = new SecureRandom();

    private static class OauthProfile {
        final String oauthId;
        final String baseName;
        OauthProfile(String oauthId, String baseName) {
            this.oauthId = oauthId;
            this.baseName = baseName;
        }
    }

    private OauthProfile extractProfile(String provider, OAuth2User oAuth2User) {
        Map<String, Object> attrs = oAuth2User.getAttributes();

        switch (provider) {
            case "google" -> {
                String sub  = (String) attrs.get("sub");
                String name = (String) attrs.get("name");
                return new OauthProfile(sub, name);
            }
            case "kakao" -> {
                String id = String.valueOf(attrs.get("id"));
                String name = null;
                Object acc = attrs.get("kakao_account");
                if (acc instanceof Map<?, ?> accMap) {
                    Object prof = accMap.get("profile");
                    if (prof instanceof Map<?, ?> p) {
                        name = (String) p.get("nickname");
                    }
                    if (name == null || name.isBlank()) {
                        String email = (String) accMap.get("email");
                        if (email != null && !email.isBlank()) {
                            name = email.split("@")[0];
                        }
                    }
                }
                return new OauthProfile(id, name);
            }
            case "naver" -> {
                Object resp = attrs.get("response");
                String id = null, name = null;
                if (resp instanceof Map<?, ?> r) {
                    id   = (String) r.get("id");
                    name = (String) r.get("name");
                    if (name == null || name.isBlank()) {
                        String email = (String) r.get("email");
                        if (email != null && !email.isBlank()) {
                            name = email.split("@")[0];
                        }
                    }
                }
                return new OauthProfile(id, name);
            }
            default -> {
                return new OauthProfile(oAuth2User.getName(), (String) attrs.get("name"));
            }
        }
    }

    @Override
    public OAuth2User loadUser(OAuth2UserRequest userRequest) {
        OAuth2User oAuth2User = super.loadUser(userRequest);

        String provider = userRequest.getClientRegistration().getRegistrationId().toLowerCase();


        OauthProfile p = extractProfile(provider, oAuth2User);

        if (p.oauthId == null || p.oauthId.isBlank()) {
            throw new IllegalStateException("OAuth2 oauthId is missing for provider=" + provider);
        }

        User user = userRepository.findByProviderAndOauthId(provider, p.oauthId)
                .orElseGet(() -> {
                    String nicknameBase = (p.baseName == null || p.baseName.isBlank()) ? provider : p.baseName;
                    String nickname = makeUniqueNickname(nicknameBase);
                    String randomProfile = randomDefaultProfileImageUrl();

                    return userRepository.save(User.builder()
                            .provider(provider)
                            .oauthId(p.oauthId)
                            .nickname(nickname)
                            .role(Role.USER)
                            .profileImage(randomProfile)
                            .build());
                });

        return new CustomOAuth2User(user, oAuth2User.getAttributes());
    }

    private String randomSuffix(int length) {
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            sb.append(ALPHANUM.charAt(SECURE_RANDOM.nextInt(ALPHANUM.length())));
        }
        return sb.toString();
    }

    private String makeUniqueNickname(String baseNickname) {
        String candidate = baseNickname;
        while (userRepository.existsByNickname(candidate)) {
            String suffix = randomSuffix(5);
            candidate = baseNickname + "_" + suffix;
        }
        return candidate;
    }

    private String randomDefaultProfileImageUrl() {
        int n = ThreadLocalRandom.current().nextInt(1, 18);
        return DEFAULT_PROFILE_BASE + n + ".webp";
    }
}
