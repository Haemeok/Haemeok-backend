package com.jdc.recipe_service.service.chat;

public record SuspiciousResult(
        boolean suspicious,
        String reason
) {
    public static SuspiciousResult clean() {
        return new SuspiciousResult(false, null);
    }
}
