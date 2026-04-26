package com.jdc.recipe_service.service.chat;

import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class AnswerValidator {

    private static final List<String> LEAK_MARKERS = List.of(
            "# 4가지 원칙",
            "# 절대 금지",
            "# 답변 구조",
            "# 역할",
            "{RECIPE}",
            "IN_SCOPE",
            "OUT_OF_SCOPE",
            "UNCLEAR"
    );

    public ValidationResult validate(String answer) {
        if (answer == null || answer.isBlank()) {
            return ValidationResult.ok();
        }
        for (String marker : LEAK_MARKERS) {
            if (answer.contains(marker)) {
                return new ValidationResult(false, "potential_prompt_leak:" + marker);
            }
        }
        return ValidationResult.ok();
    }
}
