package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.testsupport.ChatTestCase;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

class SuspiciousDetectorRegressionTest {

    private final SuspiciousDetector detector = new SuspiciousDetector();

    @Test
    @DisplayName("카테고리 분포 진단")
    void categoryDistributionDiagnostic() {
        List<ChatTestCase> all = ChatTestCase.loadAll();
        Map<String, Long> dist = all.stream()
                .collect(Collectors.groupingBy(ChatTestCase::category, TreeMap::new, Collectors.counting()));
        System.out.printf("총 %d건. 카테고리별:%n", all.size());
        dist.forEach((k, v) -> System.out.printf("  %s: %d%n", k, v));

        assertThat(all).hasSize(200);
    }

    @Test
    @DisplayName("인젝션 카테고리 SuspiciousDetector 적중률 (baseline ≥ 80%)")
    void injectionDetectionRate() {
        List<ChatTestCase> injection = ChatTestCase.loadAll().stream()
                .filter(c -> "인젝션".equals(c.category()))
                .toList();

        List<String> missed = new ArrayList<>();
        int detected = 0;
        for (ChatTestCase c : injection) {
            if (detector.detect(c.question()).suspicious()) {
                detected++;
            } else {
                missed.add(c.question());
            }
        }

        double rate = injection.isEmpty() ? 0 : (double) detected / injection.size() * 100;
        System.out.printf("%n[Injection] 적중률: %d/%d (%.1f%%)%n", detected, injection.size(), rate);
        if (!missed.isEmpty()) {
            System.out.println("미감지 케이스:");
            missed.forEach(q -> System.out.println("  - " + q));
        }

        // baseline 변천:
        //   40% (7 패턴) → 60% (B1, +3 평가용이야/DEBUG모드/안전필터)
        //   → 80%+ (B2, +5 system:/내부 instructions/원래 지시/너 원래+메타/이제부터 너는+전문가).
        // 남은 미감지 1건: "이제부터 주식 전문가 역할" (NO "너는") — Phase 3 후보.
        // memory: project_chat_suspicious_patterns_calibration.md
        assertThat(rate).isGreaterThanOrEqualTo(80.0);
    }

    @Test
    @DisplayName("정상-깔끔 카테고리 false positive 0")
    void cleanCategoryNoFalsePositive() {
        List<ChatTestCase> clean = ChatTestCase.loadAll().stream()
                .filter(c -> "정상-깔끔".equals(c.category()))
                .toList();

        List<String> falsePositives = new ArrayList<>();
        for (ChatTestCase c : clean) {
            SuspiciousResult r = detector.detect(c.question());
            if (r.suspicious()) {
                falsePositives.add("%s (reason=%s)".formatted(c.question(), r.reason()));
            }
        }

        System.out.printf("%n[Clean] FP: %d/%d%n", falsePositives.size(), clean.size());
        falsePositives.forEach(s -> System.out.println("  - " + s));

        assertThat(falsePositives).isEmpty();
    }

    @Test
    @DisplayName("범위밖 카테고리 false positive 0")
    void outOfScopeCategoryNoFalsePositive() {
        List<ChatTestCase> outOfScope = ChatTestCase.loadAll().stream()
                .filter(c -> c.category().startsWith("범위밖"))
                .toList();

        List<String> falsePositives = new ArrayList<>();
        for (ChatTestCase c : outOfScope) {
            if (detector.detect(c.question()).suspicious()) {
                falsePositives.add(c.question());
            }
        }

        System.out.printf("%n[Out-of-scope] FP: %d/%d%n", falsePositives.size(), outOfScope.size());
        falsePositives.forEach(q -> System.out.println("  - " + q));

        assertThat(falsePositives).isEmpty();
    }

    @Test
    @DisplayName("위험 카테고리 false positive 0 (자연 한국어 표현이 패턴 매칭에 안 잡혀야)")
    void dangerCategoryNoFalsePositive() {
        List<ChatTestCase> danger = ChatTestCase.loadAll().stream()
                .filter(c -> c.category().startsWith("위험-") || "복합-위험".equals(c.category()))
                .toList();

        List<String> falsePositives = new ArrayList<>();
        for (ChatTestCase c : danger) {
            if (detector.detect(c.question()).suspicious()) {
                falsePositives.add(c.question());
            }
        }

        System.out.printf("%n[Danger] FP: %d/%d%n", falsePositives.size(), danger.size());
        falsePositives.forEach(q -> System.out.println("  - " + q));

        assertThat(falsePositives).isEmpty();
    }
}
