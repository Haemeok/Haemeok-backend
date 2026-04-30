package com.jdc.recipe_service.service.ingredient.normalize;

import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Set;

/**
 * 자연어 수량 문자열 → {@link ParsedQuantity}.
 *
 * <p>Pure component (no DB, no state). raw-preserving 정책에 따라 변환 실패도 예외 대신
 * 결과 객체로 표현한다 — 호출자가 PARTIAL/UNRESOLVED 분기를 결정.
 *
 * <p>지원 형식: 정수, 소수("0.5"), 단순분수("1/2"), 정성표현("약간"). 혼합분수/공학표기는 지원 안 함.
 */
@Component
public class QuantityParser {

    /** "약간"류 정성 수량 — 수량은 없지만 입력 누락은 아님. */
    private static final Set<String> SPECIAL_QUALIFIERS = Set.of(
            "약간", "적당량", "조금", "한줌", "한 줌", "취향껏"
    );

    /** 분수 division 결과 정밀도. 6자리면 1/3=0.333333까지 보존. */
    private static final int FRACTION_SCALE = 6;

    public ParsedQuantity parse(String raw) {
        if (raw == null) return ParsedQuantity.blank(null);
        String trimmed = raw.trim();
        if (trimmed.isEmpty()) return ParsedQuantity.blank(raw);

        if (SPECIAL_QUALIFIERS.contains(trimmed)) {
            return new ParsedQuantity(null, true, raw);
        }

        if (trimmed.contains("/")) {
            BigDecimal frac = tryParseFraction(trimmed);
            if (frac != null) return new ParsedQuantity(frac, false, raw);
        }

        try {
            return new ParsedQuantity(new BigDecimal(trimmed), false, raw);
        } catch (NumberFormatException e) {
            return ParsedQuantity.blank(raw);
        }
    }

    private BigDecimal tryParseFraction(String text) {
        String[] parts = text.split("/");
        if (parts.length != 2) return null;
        try {
            BigDecimal numerator = new BigDecimal(parts[0].trim());
            BigDecimal denominator = new BigDecimal(parts[1].trim());
            if (denominator.signum() == 0) return null;
            return numerator.divide(denominator, FRACTION_SCALE, RoundingMode.HALF_UP).stripTrailingZeros();
        } catch (NumberFormatException e) {
            return null;
        }
    }
}
