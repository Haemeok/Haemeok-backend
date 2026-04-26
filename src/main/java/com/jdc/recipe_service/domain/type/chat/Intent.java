package com.jdc.recipe_service.domain.type.chat;

public enum Intent {
    IN_SCOPE,
    OUT_OF_SCOPE,
    UNCLEAR,
    UNKNOWN;

    public static Intent fromString(String label) {
        if (label == null) return UNKNOWN;
        String normalized = label.trim().toUpperCase();
        for (Intent intent : values()) {
            if (normalized.contains(intent.name())) {
                return intent;
            }
        }
        return UNKNOWN;
    }
}
