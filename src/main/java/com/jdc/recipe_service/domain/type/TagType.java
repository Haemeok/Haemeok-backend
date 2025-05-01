package com.jdc.recipe_service.domain.type;

import lombok.Getter;
import org.apache.coyote.BadRequestException;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

@Getter
public enum TagType {

    home_party("🏠 홈파티"),
    picnic("🌼 피크닉"),
    camping("🏕️ 캠핑"),
    healthy("🥗 다이어트 / 건강식"),
    kids("👶 아이와 함께"),
    solo("🍽️ 혼밥"),
    drink("🍶 술안주"),
    brunch("🥐 브런치"),
    late_night("🌙 야식"),
    quick("⚡ 초스피드 / 간단 요리"),
    holiday("🎉 기념일 / 명절"),
    lunchbox("🍱 도시락"),
    air_fryer("🔌에어프라이어"),
    hangover("🍲 해장");

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

    public static TagType fromNameOrThrow(String tagName) {
        try {
            return TagType.valueOf(tagName);
        } catch (IllegalArgumentException e) {
            // 1) IllegalArgumentException 을 바로 던지고
            // throw new IllegalArgumentException("잘못된 태그 이름입니다: " + tagName, e);

            // 또는 2) 스프링 예외로 바로 매핑하고 싶다면 아래와 같이:
            throw new ResponseStatusException(
                    HttpStatus.BAD_REQUEST,
                    "잘못된 태그 이름입니다: " + tagName,
                    e
            );
        }
    }
}
