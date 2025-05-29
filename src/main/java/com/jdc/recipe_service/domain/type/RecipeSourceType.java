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
            return USER;
        }
    }
}
