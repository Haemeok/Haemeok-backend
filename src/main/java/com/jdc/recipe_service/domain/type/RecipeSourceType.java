package com.jdc.recipe_service.domain.type;

import lombok.Getter;

@Getter
public enum RecipeSourceType {
    USER,
    AI;

    public static RecipeSourceType fromNullable(String source) {
        if (source == null) return USER;
        try {
            return RecipeSourceType.valueOf(source.toUpperCase());
        } catch (IllegalArgumentException e) {
            return USER; // 잘못된 값이 들어와도 기본값 USER
        }
    }
}
