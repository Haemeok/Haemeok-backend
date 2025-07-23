package com.jdc.recipe_service.domain.type;

import lombok.Getter;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import java.util.Arrays;

@Getter
public enum TagType {

    HOME_PARTY("🏠 홈파티"),
    PICNIC("🌼 피크닉"),
    CAMPING("🏕️ 캠핑"),
    HEALTHY("🥗 다이어트 / 건강식"),
    KIDS("👶 아이와 함께"),
    SOLO("🍽️ 혼밥"),
    DRINK("🍶 술안주"),
    BRUNCH("🥐 브런치"),
    LATE_NIGHT("🌙 야식"),
    QUICK("⚡ 초스피드 / 간단 요리"),
    HOLIDAY("🎉 기념일 / 명절"),
    LUNCHBOX("🍱 도시락"),
    AIR_FRYER("🔌 에어프라이어"),
    HANGOVER("🍲 해장");

    private final String displayName;

    TagType(String displayName) {
        this.displayName = displayName;
    }

    public static TagType fromDisplayName(String name) {
        for (TagType type : TagType.values()) {
            if (type.displayName.equals(name)) {
                return type;
            }
        }
        throw new IllegalArgumentException("존재하지 않는 태그입니다: " + name);
    }

    public static TagType fromCode(String code) {
        return Arrays.stream(TagType.values())
                .filter(t -> t.name().equalsIgnoreCase(code))
                .findFirst()
                .orElseThrow(() -> new ResponseStatusException(
                        HttpStatus.BAD_REQUEST,
                        "잘못된 태그 이름입니다: " + code
                ));
    }

    public static TagType fromNameOrThrow(String tagName) {
        try {
            return TagType.valueOf(tagName);
        } catch (IllegalArgumentException e) {
            throw new ResponseStatusException(
                    HttpStatus.BAD_REQUEST,
                    "잘못된 태그 이름입니다: " + tagName,
                    e
            );
        }
    }
}
