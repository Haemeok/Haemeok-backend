package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.testsupport.ChatTestCase;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class AnswerValidatorRegressionTest {

    private final AnswerValidator validator = new AnswerValidator();

    @Test
    @DisplayName("200건 답변 모두 누설 마커 없음 (Pro 답변 운영 데이터 검증)")
    void noPromptLeakInRealAnswers() {
        List<ChatTestCase> all = ChatTestCase.loadAll();

        List<String> leaked = new ArrayList<>();
        int evaluated = 0;
        for (ChatTestCase c : all) {
            if (c.answer() == null || c.answer().isBlank()) continue;
            evaluated++;
            ValidationResult r = validator.validate(c.answer());
            if (!r.valid()) {
                leaked.add("[#%d|%s] reason=%s | answer=%s"
                        .formatted(c.seq(), c.category(), r.reason(), c.answer().substring(0, Math.min(80, c.answer().length()))));
            }
        }

        System.out.printf("%n[Validator regression] 누설 감지: %d/%d (평가 대상)%n", leaked.size(), evaluated);
        leaked.forEach(System.out::println);

        assertThat(leaked).isEmpty();
    }

    @Test
    @DisplayName("위험 카테고리 답변 false positive 0 (자연 한국어 '절대 금지' 등 통과)")
    void dangerCategoryNoFalsePositive() {
        List<ChatTestCase> danger = ChatTestCase.loadAll().stream()
                .filter(c -> c.category().startsWith("위험-") || "복합-위험".equals(c.category()))
                .toList();

        List<String> falsePositives = new ArrayList<>();
        for (ChatTestCase c : danger) {
            if (c.answer() == null || c.answer().isBlank()) continue;
            ValidationResult r = validator.validate(c.answer());
            if (!r.valid()) {
                falsePositives.add("[#%d] reason=%s | %s"
                        .formatted(c.seq(), r.reason(), c.answer().substring(0, Math.min(60, c.answer().length()))));
            }
        }

        System.out.printf("%n[Danger validator] FP: %d/%d%n", falsePositives.size(), danger.size());
        falsePositives.forEach(System.out::println);

        assertThat(falsePositives).isEmpty();
    }
}
