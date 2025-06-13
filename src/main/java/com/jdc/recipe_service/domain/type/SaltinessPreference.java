package com.jdc.recipe_service.domain.type;

public enum SaltinessPreference {
    LIGHT,
    NORMAL,
    STRONG;

    public String getDisplayName() {
        return switch (this) {
            case LIGHT -> "싱겁게";
            case NORMAL -> "보통";
            case STRONG -> "짭짤하게";
        };
    }
}
