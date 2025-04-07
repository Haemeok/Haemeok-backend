package com.jdc.recipe_service.domain.type;

import lombok.Getter;

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
    AIR_FRYER("🔌에어프라이어"),
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
}
