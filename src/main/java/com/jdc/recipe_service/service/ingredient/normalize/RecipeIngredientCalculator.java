package com.jdc.recipe_service.service.ingredient.normalize;

import com.jdc.recipe_service.domain.entity.Ingredient;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;

/**
 * recipe_ingredients line별 영양/원가 계산.
 *
 * <p><b>3 inclusion paths</b>
 * <ol>
 *   <li>MAPPED + normalizedGrams + ingredient.per-g 값 → grams × per-g 계산</li>
 *   <li>PARTIAL (C' bypass 포함): ingredient_id=null이어도 ingredient_unit_id가 살아있으면
 *       unit→ingredient 경로로 canonical 복원하여 계산. 호출자가 미리 unit owner ingredient를
 *       resolve해서 input에 같이 넣어준다 (DB 의존을 helper에 두지 않기 위함).</li>
 *   <li>customByUser/명시값: customCalorie/customPrice 등 명시값이 있으면 그대로 사용</li>
 * </ol>
 *
 * <p><b>보류 (pending)</b>: 위 3개 경로 어디에도 해당 안 되면 합산에서 제외하고 pendingCount만 증가.
 * 0원/0kcal로 삼키지 않는다 — "계산 불가"와 "0"은 다른 의미.
 */
@Component
public class RecipeIngredientCalculator {

    /** 가격 라인 합산 시 1원 단위 정수 라운딩. */
    private static final int PRICE_SCALE = 0;

    /** 영양 합산 시 소수 3자리. 응답 단계에서 추가 라운딩 가능. */
    private static final int NUTRITION_SCALE = 3;

    public LineCalculation calculate(CalculationLineInput line) {
        if (line == null) return LineCalculation.pending();

        // Path 1 & 2: canonical (per-g 기반). per-g + grams가 있으면 무조건 우선 — custom*는 무시.
        // 이 정책이 핵심: AI가 custom*를 줬어도 unit/ingredient seed가 들어오면 자동으로 canonical
        // 우선권이 살아남도록 함. canonical을 path 3보다 먼저 검사.
        Ingredient source = pickPerGramSource(line);
        if (source != null && line.normalizedGrams() != null && hasRequiredPerGramValues(source)) {
            return computeFromPerG(source, line.normalizedGrams());
        }

        // Path 3: AI fallback / user CUSTOM. customPrice + customCalorie 둘 다 있고,
        // 의미 있는 fallback signal이 있을 때만 included.
        // signal: ingredient_candidate_id 연결 (AI fallback) 또는 status=CUSTOM (사용자 의도).
        // 기존 row의 customPrice=0 (default)을 "0원으로 계산"되지 않게 막는 가드.
        if (hasFallbackSignal(line) && hasRequiredCustomValues(line)) {
            return new LineCalculation(
                    line.customCalorie(),
                    line.customPrice(),
                    nullToZero(line.customCarbohydrate()),
                    nullToZero(line.customProtein()),
                    nullToZero(line.customFat()),
                    nullToZero(line.customSugar()),
                    nullToZero(line.customSodium()),
                    true
            );
        }

        return LineCalculation.pending();
    }

    public CalculationSummary summarize(List<CalculationLineInput> lines) {
        int mapped = 0, partial = 0, unresolved = 0, custom = 0;
        int calculated = 0;

        BigDecimal totalCalories = BigDecimal.ZERO;
        long totalPrice = 0;
        BigDecimal totalCarb = BigDecimal.ZERO;
        BigDecimal totalProtein = BigDecimal.ZERO;
        BigDecimal totalFat = BigDecimal.ZERO;
        BigDecimal totalSugar = BigDecimal.ZERO;
        BigDecimal totalSodium = BigDecimal.ZERO;

        if (lines != null) {
            for (CalculationLineInput line : lines) {
                if (line == null) continue;
                IngredientResolutionStatus status = line.status();
                if (status == IngredientResolutionStatus.MAPPED) mapped++;
                else if (status == IngredientResolutionStatus.PARTIAL) partial++;
                else if (status == IngredientResolutionStatus.UNRESOLVED) unresolved++;
                else if (status == IngredientResolutionStatus.CUSTOM) custom++;

                LineCalculation calc = calculate(line);
                if (calc.included()) {
                    calculated++;
                    totalCalories = totalCalories.add(calc.calories());
                    totalPrice += calc.price();
                    totalCarb = totalCarb.add(calc.carbohydrate());
                    totalProtein = totalProtein.add(calc.protein());
                    totalFat = totalFat.add(calc.fat());
                    totalSugar = totalSugar.add(calc.sugar());
                    totalSodium = totalSodium.add(calc.sodium());
                }
            }
        }

        int total = mapped + partial + unresolved + custom;
        int pending = total - calculated;

        return new CalculationSummary(
                totalCalories.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                totalPrice,
                totalCarb.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                totalProtein.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                totalFat.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                totalSugar.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                totalSodium.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                mapped, partial, unresolved, custom,
                calculated, pending
        );
    }

    /**
     * Path 1 (MAPPED): canonical Ingredient 사용
     * Path 2 (PARTIAL with unit→ingredient): unit owner ingredient 사용
     */
    private Ingredient pickPerGramSource(CalculationLineInput line) {
        if (line.status() == IngredientResolutionStatus.MAPPED) {
            return line.canonicalIngredient();
        }
        if (line.status() == IngredientResolutionStatus.PARTIAL) {
            // C' bypass row 등: ingredient_id=null이지만 unit owner는 알 수 있음.
            // 호출자가 unit→ingredient를 미리 채워줘야 한다.
            return line.unitOwnerIngredient() != null
                    ? line.unitOwnerIngredient()
                    : line.canonicalIngredient();
        }
        return null;
    }

    private LineCalculation computeFromPerG(Ingredient source, BigDecimal grams) {
        BigDecimal kcal = mul(source.getKcalPerG(), grams);
        BigDecimal price = mul(source.getPricePerG(), grams);
        BigDecimal carb = mul(source.getCarbohydrateGPerG(), grams);
        BigDecimal protein = mul(source.getProteinGPerG(), grams);
        BigDecimal fat = mul(source.getFatGPerG(), grams);
        BigDecimal sugar = mul(source.getSugarGPerG(), grams);
        BigDecimal sodium = mul(source.getSodiumMgPerG(), grams);

        long priceRounded = price.setScale(PRICE_SCALE, RoundingMode.HALF_UP).longValueExact();

        return new LineCalculation(
                kcal.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                priceRounded,
                carb.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                protein.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                fat.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                sugar.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                sodium.setScale(NUTRITION_SCALE, RoundingMode.HALF_UP),
                true
        );
    }

    private static BigDecimal mul(BigDecimal perG, BigDecimal grams) {
        if (perG == null || grams == null) return BigDecimal.ZERO;
        return perG.multiply(grams);
    }

    private static BigDecimal nullToZero(BigDecimal v) {
        return v == null ? BigDecimal.ZERO : v;
    }

    /**
     * Path 3 included 조건. signal 종류에 따라 요구 필드 다름.
     *
     * <ul>
     *   <li><b>AI fallback (ingredient_candidate_id != null)</b>: 7개 custom 필드(kcal/price/탄/단/지/당/나) 모두
     *       non-null 요구. AI는 line-total을 atomic으로 추정해야 하고, 일부 macro만 누락하면 합산 영양이
     *       체계적으로 왜곡됨 — pending이 더 정확. 후속 unit/ingredient seed로 path 1/2 승격되면 자연 해소.</li>
     *   <li><b>User CUSTOM (status=CUSTOM)</b>: customCalorie + customPrice만 요구. 사용자 입력 편의를 위해
     *       macro 필드는 optional — 누락 시 0으로 합산 (legacy V1 정책 유지).</li>
     * </ul>
     */
    private static boolean hasRequiredCustomValues(CalculationLineInput line) {
        if (line.ingredientCandidateId() != null) {
            return line.customCalorie() != null
                    && line.customPrice() != null
                    && line.customCarbohydrate() != null
                    && line.customProtein() != null
                    && line.customFat() != null
                    && line.customSugar() != null
                    && line.customSodium() != null;
        }
        return line.customCalorie() != null && line.customPrice() != null;
    }

    /**
     * Path 3 진입을 위한 fallback signal — custom*가 의미 있는 데이터인지 판정.
     *
     * <ul>
     *   <li>{@code ingredient_candidate_id != null}: AI/YouTube 추출 시 missing unit/ingredient로
     *       후보가 생성되어 connector가 박힌 경우. AI가 line-total로 추정한 값.</li>
     *   <li>{@code status = CUSTOM}: 사용자가 의도적으로 마스터 매핑 거부, custom 값 직접 입력.</li>
     * </ul>
     *
     * <p>이 signal이 없으면 custom*는 default ZERO (legacy 데이터의 빈 값)일 수 있으므로
     * "0원으로 계산"하지 않고 pending. canonical 가능 row의 custom*도 path 3에 도달 전에
     * 검사되어 경합 없이 canonical이 우선.
     */
    private static boolean hasFallbackSignal(CalculationLineInput line) {
        return line.ingredientCandidateId() != null
                || line.status() == IngredientResolutionStatus.CUSTOM;
    }

    /**
     * Path 1/2 included 조건: 마스터의 per-g 값이 모두 채워져 있어야 한다.
     * 백필이 7개 필드를 atomic하게 같이 채우는 정책이지만, 코드 차원에서도 미완 마스터를
     * "0으로 흘리지 않기" 위해 방어한다 — 하나라도 null이면 pending.
     */
    private static boolean hasRequiredPerGramValues(Ingredient ingredient) {
        return ingredient.getKcalPerG() != null
                && ingredient.getPricePerG() != null
                && ingredient.getCarbohydrateGPerG() != null
                && ingredient.getProteinGPerG() != null
                && ingredient.getFatGPerG() != null
                && ingredient.getSugarGPerG() != null
                && ingredient.getSodiumMgPerG() != null;
    }

    /**
     * Calculator 입력. Repository 의존을 helper에 두지 않기 위해 호출자가 미리 resolve한
     * canonical ingredient / unit owner ingredient를 직접 넣는다.
     *
     * <ul>
     *   <li>MAPPED: canonicalIngredient만 채움. canonical path 1.</li>
     *   <li>PARTIAL with C' bypass: ingredient_id=null이지만 unitOwnerIngredient를 채움. canonical path 2.</li>
     *   <li>PARTIAL with ingredient hit + unit miss: candidate id 연결 + custom* 보존. path 3 (AI fallback).</li>
     *   <li>UNRESOLVED: candidate id 연결 + custom* 보존. path 3 (AI fallback).</li>
     *   <li>CUSTOM: customByUser=true, custom* 직접 입력. path 3 (user custom).</li>
     * </ul>
     *
     * <p><b>fallback signal</b>: {@code ingredientCandidateId}가 path 3 진입 가드. 후속 batch가
     * candidate를 RESOLVED 처리하면서 RecipeIngredient에 ingredient_unit_id/normalized_grams를
     * 채우면 자동으로 path 1/2가 우선 선택됨 — custom*는 그대로 남아 있어도 무시됨.
     */
    public record CalculationLineInput(
            IngredientResolutionStatus status,
            BigDecimal normalizedGrams,
            Ingredient canonicalIngredient,
            Ingredient unitOwnerIngredient,
            Long ingredientCandidateId,
            BigDecimal customCalorie,
            Integer customPrice,
            BigDecimal customCarbohydrate,
            BigDecimal customProtein,
            BigDecimal customFat,
            BigDecimal customSugar,
            BigDecimal customSodium
    ) {}

    public record LineCalculation(
            BigDecimal calories,
            long price,
            BigDecimal carbohydrate,
            BigDecimal protein,
            BigDecimal fat,
            BigDecimal sugar,
            BigDecimal sodium,
            boolean included
    ) {
        public static LineCalculation pending() {
            return new LineCalculation(
                    BigDecimal.ZERO, 0,
                    BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO,
                    BigDecimal.ZERO, BigDecimal.ZERO,
                    false
            );
        }
    }

    public record CalculationSummary(
            BigDecimal totalCalories,
            long totalIngredientCost,
            BigDecimal totalCarbohydrate,
            BigDecimal totalProtein,
            BigDecimal totalFat,
            BigDecimal totalSugar,
            BigDecimal totalSodium,
            int mappedCount,
            int partialCount,
            int unresolvedCount,
            int customCount,
            int calculatedCount,
            int pendingCount
    ) {}
}
