package com.jdc.recipe_service.security.oauth;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.Role;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.oauth2.client.userinfo.DefaultOAuth2UserService;
import org.springframework.security.oauth2.client.userinfo.OAuth2UserRequest;
import org.springframework.security.oauth2.core.user.DefaultOAuth2User;
import org.springframework.security.oauth2.core.user.OAuth2User;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.Collections;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

@Slf4j
@Service
@RequiredArgsConstructor
public class CustomOAuth2UserService extends DefaultOAuth2UserService {

    private final UserRepository userRepository;
    private final ObjectMapper objectMapper = new ObjectMapper();
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
            case "apple" -> {
                String sub = (String) attrs.get("sub");
                String email = (String) attrs.get("email");

                String name = null;
                if (email != null && !email.isBlank()) {
                    name = email.split("@")[0];
                }
                return new OauthProfile(sub, name);
            }
            default -> {
                return new OauthProfile(oAuth2User.getName(), (String) attrs.get("name"));
            }
        }
    }

    @Override
    public OAuth2User loadUser(OAuth2UserRequest userRequest) {
        String provider = userRequest.getClientRegistration().getRegistrationId().toLowerCase();

        OAuth2User oAuth2User;

        if ("apple".equals(provider)) {
            oAuth2User = processAppleUser(userRequest);
        } else {
            oAuth2User = super.loadUser(userRequest);
        }

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

    private OAuth2User processAppleUser(OAuth2UserRequest userRequest) {
        String idToken = (String) userRequest.getAdditionalParameters().get("id_token");
        Map<String, Object> attributes = decodeJwtTokenPayload(idToken);

        return new DefaultOAuth2User(
                Collections.singleton(new SimpleGrantedAuthority("ROLE_USER")),
                attributes,
                "sub"
        );
    }

    private Map<String, Object> decodeJwtTokenPayload(String jwtToken) {
        try {
            String[] parts = jwtToken.split("\\.");
            Base64.Decoder decoder = Base64.getUrlDecoder();
            String payload = new String(decoder.decode(parts[1]), StandardCharsets.UTF_8);
            return objectMapper.readValue(payload, Map.class);
        } catch (Exception e) {
            log.error("Apple ID Token Parsing Failed", e);
            throw new RuntimeException("Apple ID Token Decode Error", e);
        }
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
