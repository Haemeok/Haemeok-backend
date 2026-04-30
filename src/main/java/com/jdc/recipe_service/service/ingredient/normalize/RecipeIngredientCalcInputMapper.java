package com.jdc.recipe_service.service.ingredient.normalize;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.CalculationLineInput;

import java.math.BigDecimal;
import java.util.Locale;
import java.util.Map;

/**
 * {@link RecipeIngredient} entity → {@link CalculationLineInput} 변환 helper.
 *
 * <p>read 단(DevRecipeDetailService)과 aggregate 백필(RecipeAggregateBackfillService) 둘 다 같은
 * 정책으로 변환해야 detail 화면 합산값과 recipe aggregate(검색/필터)가 일관된다 — 두 곳에서 변환
 * 로직이 어긋나면 detail은 정확한데 목록 검색은 stale인 상황이 생긴다.
 *
 * <h3>핵심 분기</h3>
 * <ul>
 *   <li><b>parseStatus</b>: resolution_status 문자열을 enum으로. 미설정/dirty면 ingredient_id 유무로
 *       추정 fallback. C' bypass row(ingredient_id=null + ingredient_unit_id != null)는 PARTIAL로
 *       우선 분류해 calculator path 2(unit→ingredient 복원)가 작동하게 한다.</li>
 *   <li><b>trustExplicitZero</b>: ingredient_candidate_id 박힌 row는 AI fallback이 line-total을 atomic하게
 *       명시한 row → 0을 정상 명시값으로 신뢰. candidate id 없는 legacy/default row는 0이 누락 가능성이라
 *       null로 보호 (calculator의 path 3 included 가드를 통과하지 못해 pending 처리).</li>
 * </ul>
 *
 * <p><b>호출자 책임</b>: {@code unitsById}는 호출자가 미리 prefetch한다 (LAZY 차단). null/empty여도 안전.
 */
public final class RecipeIngredientCalcInputMapper {

    private RecipeIngredientCalcInputMapper() {}

    public static CalculationLineInput toInput(RecipeIngredient e, Map<Long, IngredientUnit> unitsById) {
        IngredientResolutionStatus status = parseStatus(e);
        Ingredient canonical = e.getIngredient();

        Ingredient unitOwner = null;
        Long unitId = e.getIngredientUnitId();
        if (unitId != null && unitsById != null) {
            IngredientUnit unit = unitsById.get(unitId);
            if (unit != null) unitOwner = unit.getIngredient();  // 호출자가 EntityGraph로 fetch
        }

        // ingredient_candidate_id가 박힌 row = AI fallback이 line-total을 atomic하게 명시한 row.
        // 0은 "정상 명시값"으로 신뢰 — nonZeroOrNull 변환을 풀어 0 그대로 calculator로 전달.
        // candidate id 없는 row(legacy 기본 ZERO 또는 user CUSTOM의 0 default)는 0을 신뢰할 수 없어 null로 보호.
        boolean trustExplicitZero = e.getIngredientCandidateId() != null;

        BigDecimal customCalorie = trustExplicitZero ? e.getCustomCalorie() : nonZeroOrNull(e.getCustomCalorie());
        Integer customPrice = trustExplicitZero ? e.getCustomPrice() : nonZeroOrNull(e.getCustomPrice());
        BigDecimal customCarbohydrate = trustExplicitZero ? e.getCustomCarbohydrate() : nonZeroOrNull(e.getCustomCarbohydrate());
        BigDecimal customProtein = trustExplicitZero ? e.getCustomProtein() : nonZeroOrNull(e.getCustomProtein());
        BigDecimal customFat = trustExplicitZero ? e.getCustomFat() : nonZeroOrNull(e.getCustomFat());
        BigDecimal customSugar = trustExplicitZero ? e.getCustomSugar() : nonZeroOrNull(e.getCustomSugar());
        BigDecimal customSodium = trustExplicitZero ? e.getCustomSodium() : nonZeroOrNull(e.getCustomSodium());

        return new CalculationLineInput(
                status,
                e.getNormalizedGrams(),
                canonical,
                unitOwner,
                e.getIngredientCandidateId(),
                customCalorie, customPrice,
                customCarbohydrate, customProtein, customFat,
                customSugar, customSodium
        );
    }

    /**
     * resolution_status String → enum. unknown/null이면 ingredient/unit 유무로 추정.
     *
     * <p><b>C' bypass row 보호</b>: ingredient_id=null이면서 ingredient_unit_id 살아있으면 PARTIAL을 우선
     * 판정 — fallback에서 customName 검사를 먼저 하면 CUSTOM으로 잘못 분류되어 calculator path 2 미작동 →
     * 0이 아니라 pending으로 흘러 사용자에게 "계산 안 된 라인"으로 보임.
     */
    public static IngredientResolutionStatus parseStatus(RecipeIngredient e) {
        String s = e.getResolutionStatus();
        if (s != null) {
            String normalized = s.trim().toUpperCase(Locale.ROOT);
            if (!normalized.isEmpty()) {
                try {
                    return IngredientResolutionStatus.valueOf(normalized);
                } catch (IllegalArgumentException ignored) {
                    // unknown 라벨은 fallback 추정으로
                }
            }
        }
        if (e.getIngredient() == null) {
            if (e.getIngredientUnitId() != null) {
                return IngredientResolutionStatus.PARTIAL;  // C' bypass
            }
            return e.getCustomName() != null
                    ? IngredientResolutionStatus.CUSTOM
                    : IngredientResolutionStatus.UNRESOLVED;
        }
        return IngredientResolutionStatus.MAPPED;
    }

    private static BigDecimal nonZeroOrNull(BigDecimal v) {
        return (v == null || v.signum() == 0) ? null : v;
    }

    private static Integer nonZeroOrNull(Integer v) {
        return (v == null || v == 0) ? null : v;
    }
}
