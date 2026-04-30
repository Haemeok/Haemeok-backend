package com.jdc.recipe_service.service.ingredient.normalize;

import org.springframework.stereotype.Component;

import java.util.Locale;
import java.util.Map;

/**
 * 자연어 단위 → ingredient_units.normalized_unit_label 매칭용 canonical 형태.
 *
 * <p>1.1 범위는 "최소 케이스" — 영문/대소문자/공백 정리와 잘 알려진 단위 alias만 처리한다.
 * 한글 단위(개/쪽/큰술/작은술 등)는 그대로 통과시킨다 (이미 canonical로 간주).
 * alias를 더 넓히는 것은 ingredient_units 데이터를 보고 1.2/1.3에서 점진 확대.
 */
@Component
public class UnitNormalizer {

    /**
     * 단일 문자 case-sensitive alias — 조리 표기 관습 (T=큰술, t=작은술).
     * lowercase 통일 전에 먼저 검사.
     */
    private static final Map<String, String> CASE_SENSITIVE_ALIAS = Map.of(
            "T", "큰술",
            "t", "작은술"
    );

    /**
     * 영문/약식 → 한글 canonical alias. 키는 lowercase trimmed.
     * 한글 unit은 alias 매핑 대신 raw 그대로 통과.
     */
    private static final Map<String, String> ALIAS = Map.ofEntries(
            // 큰술/작은술 계열 (다중 문자만 — 단일 문자 T/t는 위에서 case-sensitive로 처리)
            Map.entry("ts", "작은술"),
            Map.entry("tbs", "큰술"),
            Map.entry("tbsp", "큰술"),
            Map.entry("tablespoon", "큰술"),
            Map.entry("tsp", "작은술"),
            Map.entry("teaspoon", "작은술"),
            // 컵/잔
            Map.entry("cup", "컵"),
            // 무게/부피 표준 단위는 lowercase 통일만
            Map.entry("g", "g"),
            Map.entry("kg", "kg"),
            Map.entry("mg", "mg"),
            Map.entry("ml", "ml"),
            Map.entry("l", "l"),
            Map.entry("cc", "ml")
    );

    /**
     * @return canonical 단위 라벨. blank 입력은 빈 문자열로 정규화.
     */
    public String normalize(String raw) {
        if (raw == null) return "";
        String trimmed = raw.trim();
        if (trimmed.isEmpty()) return "";

        // 1) 단일 문자 case-sensitive 우선: T → 큰술, t → 작은술
        String exactMatch = CASE_SENSITIVE_ALIAS.get(trimmed);
        if (exactMatch != null) return exactMatch;

        // 2) 일반 alias는 lowercase로 매핑
        String key = trimmed.toLowerCase(Locale.ROOT);
        String mapped = ALIAS.get(key);
        if (mapped != null) return mapped;

        // 3) alias 미스: 한글이면 raw 보존, 영문이면 lowercase로만 정규화.
        if (containsHangul(trimmed)) return trimmed;
        return key;
    }

    private static boolean containsHangul(String s) {
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c >= 0xAC00 && c <= 0xD7A3) return true;
            if (c >= 0x1100 && c <= 0x11FF) return true;
            if (c >= 0x3130 && c <= 0x318F) return true;
        }
        return false;
    }
}
