package com.jdc.recipe_service.service.ingredient.normalize;

import java.math.BigDecimal;

/**
 * 수량 문자열 파싱 결과.
 *
 * <ul>
 *   <li>{@code amount} non-null: 숫자로 파싱 성공 (3, 0.5, 1/2 → 0.5 등)</li>
 *   <li>{@code amount} null + {@code special}=true: "약간"/"적당량" 같은 정성적 표현.
 *       의도적으로 수량을 안 적은 것이지 입력 누락이 아님 → 계산은 보류, raw는 보존.</li>
 *   <li>{@code amount} null + {@code special}=false: blank 또는 파싱 실패. 입력 자체가 없거나 비정상.</li>
 * </ul>
 */
public record ParsedQuantity(BigDecimal amount, boolean special, String raw) {

    public static ParsedQuantity blank(String raw) {
        return new ParsedQuantity(null, false, raw);
    }

    public boolean hasAmount() {
        return amount != null;
    }
}
