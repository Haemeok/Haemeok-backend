package com.jdc.recipe_service.service.chat;

import org.springframework.stereotype.Component;

import java.util.List;
import java.util.regex.Pattern;

@Component
public class SuspiciousDetector {

    private record InjectionPattern(Pattern pattern, String reason) {}

    private static final List<InjectionPattern> INJECTION_PATTERNS = List.of(
            new InjectionPattern(Pattern.compile("(?i)ignore\\s+(all\\s+)?previous"), "ignore_previous"),
            new InjectionPattern(Pattern.compile("(?i)system\\s*prompt"),               "system_prompt_reveal"),
            new InjectionPattern(Pattern.compile("(?i)repeat\\s+(above|the)\\s+instructions"), "repeat_instructions"),
            new InjectionPattern(Pattern.compile("이전\\s*지시"),                        "ignore_previous_kr"),
            new InjectionPattern(Pattern.compile("너는\\s*이제"),                        "role_switch_kr"),
            new InjectionPattern(Pattern.compile("프롬프트\\s*(공개|보여)"),             "prompt_reveal_kr"),
            // Tier 1 보강 (B1, 2026-04-26): 200건 회귀에서 자주 시도된 미감지 3건.
            new InjectionPattern(Pattern.compile("평가용이야"),                          "evaluation_pretext"),
            new InjectionPattern(Pattern.compile("(?i)DEBUG\\s*모드"),                   "debug_mode_request"),
            new InjectionPattern(Pattern.compile("안전\\s*필터"),                        "safety_filter_disable"),
            // Tier 2 보강 (B2, 2026-04-26): 안전 3 + 신중 2. FP 0 유지 검증됨.
            // 참고: B0의 `이제부터\s*너는` (broad) 은 B2 #5 (narrower)가 흡수해서 제거됨.
            new InjectionPattern(Pattern.compile("(?i)^\\s*system\\s*:"),                "system_directive_injection"),
            new InjectionPattern(Pattern.compile("(?i)내부\\s*instructions"),            "internal_instructions_reveal"),
            new InjectionPattern(Pattern.compile("(?i)원래\\s*지시"),                    "original_directive_override"),
            new InjectionPattern(Pattern.compile("(?i)너\\s*원래\\s*(역할|뭐|무엇|기능|뭐였)"), "meta_role_inquiry"),
            new InjectionPattern(Pattern.compile("(?i)이제부터\\s*너는\\s*.{0,15}?(전문가|역할)"), "role_replacement")
    );

    public SuspiciousResult detect(String question) {
        if (question == null || question.isBlank()) {
            return SuspiciousResult.clean();
        }
        for (InjectionPattern entry : INJECTION_PATTERNS) {
            if (entry.pattern().matcher(question).find()) {
                return new SuspiciousResult(true, "injection_" + entry.reason());
            }
        }
        return SuspiciousResult.clean();
    }
}
