package com.jdc.recipe_service.domain.type.chat;

import com.jdc.recipe_service.testsupport.ChatTestCase;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class IntentRegressionTest {

    @Test
    @DisplayName("200건 Mini분류 결과 Intent.fromString이 모두 valid (UNKNOWN 0)")
    void allMiniLabelsParsedSafely() {
        List<ChatTestCase> all = ChatTestCase.loadAll();

        List<String> unknowns = new ArrayList<>();
        for (ChatTestCase c : all) {
            if (c.miniLabel() == null) continue;
            Intent parsed = Intent.fromString(c.miniLabel());
            if (parsed == Intent.UNKNOWN) {
                unknowns.add("[#%d] mini=%s".formatted(c.seq(), c.miniLabel()));
            }
        }

        System.out.printf("%n[Intent regression] UNKNOWN: %d/%d%n", unknowns.size(), all.size());
        unknowns.forEach(System.out::println);

        assertThat(unknowns).isEmpty();
    }

    @Test
    @DisplayName("200건 분류 정확도 baseline (≥90%, 검증된 93% 재현)")
    void classificationAccuracyBaseline() {
        List<ChatTestCase> all = ChatTestCase.loadAll();
        long correct = all.stream().filter(ChatTestCase::classifiedCorrectly).count();
        double rate = (double) correct / all.size() * 100;

        System.out.printf("%n[Mini accuracy] %d/%d (%.1f%%)%n", correct, all.size(), rate);

        assertThat(rate).isGreaterThanOrEqualTo(90.0);
    }
}
