package com.jdc.recipe_service.service.chat;

public record ValidationResult(
        boolean valid,
        String reason
) {
    public static ValidationResult ok() {
        return new ValidationResult(true, null);
    }
}
