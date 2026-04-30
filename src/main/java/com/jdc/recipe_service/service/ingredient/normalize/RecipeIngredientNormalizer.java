package com.jdc.recipe_service.service.ingredient.normalize;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * raw → resolved 한 batch 변환.
 *
 * <p><b>1.1 정책 (C' 임시)</b>: UNIQUE(recipe_id, ingredient_id) 제약 아래 안전 저장.
 * <ul>
 *   <li>같은 ingredient + 같은 unit: 첫 row에 amount/normalizedGrams 합산 (merge)</li>
 *   <li>같은 ingredient + 다른 unit: 두 번째 이후는 C' bypass — ingredient_id=null,
 *       단, ingredient_unit_id는 보존하여 calculator가 unit→ingredient로 복원 가능</li>
 *   <li>매칭 실패: PARTIAL (이름만 매칭) 또는 UNRESOLVED (이름 매칭 실패)</li>
 *   <li>customByUser=true: CUSTOM (마스터 매칭 시도 안 함)</li>
 * </ul>
 *
 * <p><b>repository 의존 없음</b>: 호출자가 미리 prefetch한 lookup map을 입력으로 받는다.
 * batch lookup 책임은 1.3 service 계층.
 */
@Component
public class RecipeIngredientNormalizer {

    private final QuantityParser quantityParser;
    private final UnitNormalizer unitNormalizer;
    private final IngredientUnitResolver unitResolver;

    public RecipeIngredientNormalizer(QuantityParser quantityParser,
                                      UnitNormalizer unitNormalizer,
                                      IngredientUnitResolver unitResolver) {
        this.quantityParser = quantityParser;
        this.unitNormalizer = unitNormalizer;
        this.unitResolver = unitResolver;
    }

    /**
     * 이름 lookup용 canonical form. 호출자가 ingredient lookup map의 키도 동일 함수로 준비해야 한다.
     *
     * <p>모든 공백 문자(스페이스/탭/기타 whitespace)는 제거한다.
     * "청 양 고추" / "청양고추" / "청양 고추" 같은 자연어 입력 변형을 같은 키로 흡수.
     */
    public static String canonicalizeName(String raw) {
        if (raw == null) return "";
        return raw.replaceAll("\\s+", "").toLowerCase(Locale.ROOT);
    }

    /**
     * @param inputs 원문 입력 리스트 (입력 순서가 dedupe 우선순위를 결정)
     * @param ingredientByLookupName 캐릭터라이즈된 이름 → Ingredient
     * @param unitsByIngredientId    Ingredient ID → 그 ingredient의 IngredientUnit 후보 리스트
     */
    public List<NormalizedLine> normalize(List<RawIngredientInput> inputs,
                                          Map<String, Ingredient> ingredientByLookupName,
                                          Map<Long, List<IngredientUnit>> unitsByIngredientId) {
        if (inputs == null || inputs.isEmpty()) return List.of();

        List<NormalizedLine> result = new ArrayList<>();

        // dedupe 추적: ingredient_id가 이미 한 번 점유되었는지, 그리고 mergeable MAPPED 라인의 위치
        Map<Long, Integer> firstClaimIndex = new HashMap<>();           // ingredient_id → 첫 점유 라인 인덱스
        Map<MergeKey, Integer> mappedIndexByKey = new HashMap<>();      // (ingredient_id, unit_id) → mappable MAPPED 인덱스

        for (RawIngredientInput input : inputs) {
            if (input == null) continue;

            // CUSTOM: 마스터 매칭 자체를 건너뜀
            if (input.customByUser()) {
                result.add(buildCustom(input));
                continue;
            }

            ParsedQuantity pq = quantityParser.parse(input.rawQuantityText());
            String normalizedUnit = unitNormalizer.normalize(input.rawUnitText());
            String lookupKey = canonicalizeName(input.rawName());

            Ingredient ingredient = lookupKey.isEmpty()
                    ? null
                    : (ingredientByLookupName == null ? null : ingredientByLookupName.get(lookupKey));

            // UNRESOLVED: 이름 매칭 실패
            if (ingredient == null) {
                result.add(buildUnresolved(input, pq));
                continue;
            }

            // unit 매칭 시도. unitsByIngredientId가 ingredient_id → units로 이미 grouping된 상태이므로
            // candidate 리스트가 곧 ingredient-pre-filtered. resolver는 label만 비교 (LAZY 차단).
            List<IngredientUnit> unitCandidates = unitsByIngredientId == null
                    ? List.of()
                    : unitsByIngredientId.getOrDefault(ingredient.getId(), List.of());
            IngredientUnit matchedUnit = unitResolver.resolve(normalizedUnit, unitCandidates).orElse(null);

            // dedupe 분기
            Integer firstIdx = firstClaimIndex.get(ingredient.getId());
            if (firstIdx != null) {
                // 같은 ingredient_id에 이미 다른 row가 있음
                if (matchedUnit != null) {
                    MergeKey mk = new MergeKey(ingredient.getId(), matchedUnit.getId());
                    Integer mergedIdx = mappedIndexByKey.get(mk);
                    if (mergedIdx != null) {
                        // (1) 같은 ingredient + 같은 unit:
                        //   - 둘 다 amount 파싱된 경우만 첫 MAPPED row에 합산 (raw 표시값도 합계로 갱신)
                        //   - 한쪽이 정성/blank("약간" 등)이면 합산 불가 — 두 번째 row를 bypass PARTIAL로 따로 보존
                        NormalizedLine existing = result.get(mergedIdx);
                        if (canMergeAmounts(existing, pq)) {
                            BigDecimal newGrams = computeGrams(pq, matchedUnit);
                            result.set(mergedIdx, mergeMappedAmounts(existing, pq, newGrams));
                            continue;
                        }
                        // 합산 불가 — bypass로 따로 보존 (두 번째 row가 사라지면 안 됨)
                    }
                }
                // (2) 같은 ingredient + 다른 unit / unit miss / 합산 불가: C' bypass PARTIAL.
                // 이미 lookup map에서 resolve한 ingredient를 직접 넘긴다 — unit.getIngredient()는
                // LAZY라 트랜잭션 밖에서 lazy load될 수 있어 위험.
                result.add(buildBypassPartial(input, pq, ingredient, matchedUnit));
                continue;
            }

            // 첫 점유: MAPPED 또는 PARTIAL(unit miss)
            firstClaimIndex.put(ingredient.getId(), result.size());

            if (matchedUnit != null) {
                BigDecimal grams = computeGrams(pq, matchedUnit);
                NormalizedLine mapped = buildMapped(input, pq, ingredient, matchedUnit, grams);
                mappedIndexByKey.put(new MergeKey(ingredient.getId(), matchedUnit.getId()), result.size());
                result.add(mapped);
            } else {
                // 이름은 hit, 단위 miss → PARTIAL (ingredient_id 보존)
                result.add(buildPartialNameHitUnitMiss(input, pq, ingredient));
            }
        }

        return result;
    }

    // ─── builders ───────────────────────────────────────────────────────────

    private NormalizedLine buildCustom(RawIngredientInput in) {
        ParsedQuantity pq = quantityParser.parse(in.rawQuantityText());
        return new NormalizedLine(
                in.rawName(), in.rawQuantityText(), in.rawUnitText(),
                true,
                pq.amount(), null,
                null, null,
                IngredientResolutionStatus.CUSTOM,
                null, null, null,
                in.customCalorie(), in.customPrice(),
                in.customCarbohydrate(), in.customProtein(), in.customFat(),
                in.customSugar(), in.customSodium(), in.customLink()
        );
    }

    private NormalizedLine buildUnresolved(RawIngredientInput in, ParsedQuantity pq) {
        return new NormalizedLine(
                in.rawName(), in.rawQuantityText(), in.rawUnitText(),
                false,
                pq.amount(), null,
                null, null,
                IngredientResolutionStatus.UNRESOLVED,
                null, null, null,
                in.customCalorie(), in.customPrice(),
                in.customCarbohydrate(), in.customProtein(), in.customFat(),
                in.customSugar(), in.customSodium(), in.customLink()
        );
    }

    private NormalizedLine buildPartialNameHitUnitMiss(RawIngredientInput in, ParsedQuantity pq,
                                                       Ingredient ingredient) {
        return new NormalizedLine(
                in.rawName(), in.rawQuantityText(), in.rawUnitText(),
                false,
                pq.amount(), null,
                ingredient.getId(), null,
                IngredientResolutionStatus.PARTIAL,
                ingredient, null, null,
                in.customCalorie(), in.customPrice(),
                in.customCarbohydrate(), in.customProtein(), in.customFat(),
                in.customSugar(), in.customSodium(), in.customLink()
        );
    }

    private NormalizedLine buildMapped(RawIngredientInput in, ParsedQuantity pq,
                                       Ingredient ingredient, IngredientUnit unit, BigDecimal grams) {
        return new NormalizedLine(
                in.rawName(), in.rawQuantityText(), in.rawUnitText(),
                false,
                pq.amount(), grams,
                ingredient.getId(), unit.getId(),
                IngredientResolutionStatus.MAPPED,
                ingredient, null, unit,
                in.customCalorie(), in.customPrice(),
                in.customCarbohydrate(), in.customProtein(), in.customFat(),
                in.customSugar(), in.customSodium(), in.customLink()
        );
    }

    /**
     * @param resolvedIngredient lookup map에서 이미 resolve한 ingredient. {@code unit.getIngredient()}
     *                           대신 이걸 사용해 LAZY 로딩을 회피.
     */
    private NormalizedLine buildBypassPartial(RawIngredientInput in, ParsedQuantity pq,
                                              Ingredient resolvedIngredient, IngredientUnit unit) {
        // ingredient_id=null (UNIQUE 회피). unit_id는 살아있으면 보존 → calc 가능.
        BigDecimal grams = unit != null ? computeGrams(pq, unit) : null;
        return new NormalizedLine(
                in.rawName(), in.rawQuantityText(), in.rawUnitText(),
                false,
                pq.amount(), grams,
                null, unit == null ? null : unit.getId(),
                IngredientResolutionStatus.PARTIAL,
                null, resolvedIngredient, unit,
                in.customCalorie(), in.customPrice(),
                in.customCarbohydrate(), in.customProtein(), in.customFat(),
                in.customSugar(), in.customSodium(), in.customLink()
        );
    }

    /**
     * Merge 가능 조건: existing/current 둘 다 amount가 파싱되어 있어야 한다.
     *
     * <p>"약간" 같은 정성 수량은 합산 불가 — 호출자가 bypass row로 따로 보존.
     */
    private static boolean canMergeAmounts(NormalizedLine existing, ParsedQuantity addPq) {
        return existing.amountValue() != null && addPq != null && addPq.amount() != null;
    }

    /**
     * 같은 (ingredient, unit) 두 row를 한 row로 합산.
     *
     * <p>amount/grams 합산뿐 아니라 raw 표시(quantity 문자열)도 합계로 갱신해야 사용자가 보는 값이
     * 일관된다. 예: "마늘 3쪽" + "마늘 5쪽" → rawQuantityText="8" (이전엔 "3"으로 잘못 남았음).
     *
     * <p>전제: 두 amount 모두 non-null (호출자가 {@link #canMergeAmounts}로 확인).
     */
    private NormalizedLine mergeMappedAmounts(NormalizedLine existing, ParsedQuantity addPq, BigDecimal addGrams) {
        BigDecimal newAmount = existing.amountValue().add(addPq.amount());
        BigDecimal newGrams = sum(existing.normalizedGrams(), addGrams);
        String mergedRawQuantity = formatAmount(newAmount);
        return new NormalizedLine(
                existing.rawName(), mergedRawQuantity, existing.rawUnitText(),
                existing.customByUser(),
                newAmount, newGrams,
                existing.ingredientId(), existing.ingredientUnitId(),
                existing.status(),
                existing.resolvedIngredient(), existing.unitOwnerIngredient(), existing.resolvedUnit(),
                existing.customCalorie(), existing.customPrice(),
                existing.customCarbohydrate(), existing.customProtein(), existing.customFat(),
                existing.customSugar(), existing.customSodium(), existing.customLink()
        );
    }

    /**
     * BigDecimal → 사용자 표시용 문자열. 정수면 정수로, 소수면 trailing zero 제거.
     */
    private static String formatAmount(BigDecimal value) {
        if (value == null) return null;
        BigDecimal stripped = value.stripTrailingZeros();
        if (stripped.scale() <= 0) {
            return stripped.toPlainString();
        }
        return stripped.toPlainString();
    }

    private static BigDecimal sum(BigDecimal a, BigDecimal b) {
        if (a == null && b == null) return null;
        if (a == null) return b;
        if (b == null) return a;
        return a.add(b);
    }

    private static BigDecimal computeGrams(ParsedQuantity pq, IngredientUnit unit) {
        if (pq == null || pq.amount() == null) return null;
        if (unit == null || unit.getEdibleGramsPerUnit() == null) return null;
        return pq.amount().multiply(unit.getEdibleGramsPerUnit())
                .setScale(3, RoundingMode.HALF_UP);
    }

    private record MergeKey(Long ingredientId, Long unitId) {}
}
