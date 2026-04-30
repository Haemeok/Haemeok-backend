package com.jdc.recipe_service.service.ingredient.normalize;

import java.math.BigDecimal;

/**
 * Normalizer 입력 — 사용자/AI/YouTube가 전달한 자연어 한 줄.
 *
 * <p>raw 보존 정책: rawName/rawQuantityText/rawUnitText는 그대로 통과한다.
 * customByUser=true면 마스터 매칭 시도 자체를 건너뛴다 (CUSTOM).
 * 명시 nutrition/price (custom*)는 path 3 계산에 쓰인다.
 */
public record RawIngredientInput(
        String rawName,
        String rawQuantityText,
        String rawUnitText,
        boolean customByUser,
        BigDecimal customCalorie,
        Integer customPrice,
        BigDecimal customCarbohydrate,
        BigDecimal customProtein,
        BigDecimal customFat,
        BigDecimal customSugar,
        BigDecimal customSodium,
        String customLink
) {
    /** 외부 호출자 편의용 — 명시값 없는 일반 입력. */
    public static RawIngredientInput of(String rawName, String rawQuantityText, String rawUnitText) {
        return new RawIngredientInput(rawName, rawQuantityText, rawUnitText, false,
                null, null, null, null, null, null, null, null);
    }

    public static RawIngredientInput custom(String rawName, String rawQuantityText, String rawUnitText) {
        return new RawIngredientInput(rawName, rawQuantityText, rawUnitText, true,
                null, null, null, null, null, null, null, null);
    }
}
