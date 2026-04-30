package com.jdc.recipe_service.service.ingredient.normalize;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

class RecipeIngredientNormalizerTest {

    private RecipeIngredientNormalizer normalizer;

    private Ingredient garlic;
    private Ingredient onion;
    private IngredientUnit garlicPiece;     // 마늘 1쪽 = 5g
    private IngredientUnit garlicSpoon;     // 마늘 1큰술 = 8g (다른 단위)
    private IngredientUnit onionPiece;      // 양파 1개 = 200g

    @BeforeEach
    void setUp() {
        normalizer = new RecipeIngredientNormalizer(
                new QuantityParser(), new UnitNormalizer(), new IngredientUnitResolver());

        garlic = Ingredient.builder().id(1L).name("마늘").build();
        onion = Ingredient.builder().id(2L).name("양파").build();

        garlicPiece = unit(10L, garlic, "쪽", "5");
        garlicSpoon = unit(11L, garlic, "큰술", "8");
        onionPiece = unit(20L, onion, "개", "200");
    }

    private static IngredientUnit unit(Long id, Ingredient ing, String label, String edibleGrams) {
        return IngredientUnit.builder()
                .id(id).ingredient(ing)
                .unitLabelKo(label).normalizedUnitLabel(label)
                .gramsPerUnit(new BigDecimal(edibleGrams))
                .edibleGramsPerUnit(new BigDecimal(edibleGrams))
                .isDefault(false)
                .build();
    }

    private Map<String, Ingredient> ingByName() {
        return Map.of("마늘", garlic, "양파", onion);
    }

    private Map<Long, List<IngredientUnit>> unitsByIng() {
        return Map.of(
                garlic.getId(), List.of(garlicPiece, garlicSpoon),
                onion.getId(), List.of(onionPiece)
        );
    }

    @Test
    @DisplayName("정상 매핑: 양파 1개 → MAPPED + grams=200 + ingredient/unit ID 채워짐")
    void normalize_mapped() {
        List<NormalizedLine> result = normalizer.normalize(
                List.of(RawIngredientInput.of("양파", "1", "개")),
                ingByName(), unitsByIng()
        );

        assertThat(result).hasSize(1);
        NormalizedLine line = result.get(0);
        assertThat(line.status()).isEqualTo(IngredientResolutionStatus.MAPPED);
        assertThat(line.ingredientId()).isEqualTo(2L);
        assertThat(line.ingredientUnitId()).isEqualTo(20L);
        assertThat(line.amountValue()).isEqualByComparingTo("1");
        assertThat(line.normalizedGrams()).isEqualByComparingTo("200.000");
        assertThat(line.rawName()).isEqualTo("양파");
        assertThat(line.rawUnitText()).isEqualTo("개");
    }

    @Test
    @DisplayName("이름 매칭 실패: 황태머리 → UNRESOLVED, ingredient_id=null")
    void normalize_unknownIngredient_unresolved() {
        List<NormalizedLine> result = normalizer.normalize(
                List.of(RawIngredientInput.of("황태머리", "1", "마리")),
                ingByName(), unitsByIng()
        );

        assertThat(result).hasSize(1);
        NormalizedLine line = result.get(0);
        assertThat(line.status()).isEqualTo(IngredientResolutionStatus.UNRESOLVED);
        assertThat(line.ingredientId()).isNull();
        assertThat(line.ingredientUnitId()).isNull();
        assertThat(line.rawName()).isEqualTo("황태머리");
        assertThat(line.amountValue()).isEqualByComparingTo("1");  // 파싱은 됐음
    }

    @Test
    @DisplayName("이름 매칭 + 단위 매칭 실패: 마늘 1봉지 → PARTIAL, ingredient_id 보존, unit_id=null")
    void normalize_knownIngredientUnknownUnit_partial() {
        List<NormalizedLine> result = normalizer.normalize(
                List.of(RawIngredientInput.of("마늘", "1", "봉지")),
                ingByName(), unitsByIng()
        );

        assertThat(result).hasSize(1);
        NormalizedLine line = result.get(0);
        assertThat(line.status()).isEqualTo(IngredientResolutionStatus.PARTIAL);
        assertThat(line.ingredientId()).isEqualTo(1L);
        assertThat(line.ingredientUnitId()).isNull();
        assertThat(line.normalizedGrams()).isNull();
        assertThat(line.isBypassRow()).isFalse();  // bypass 아님 (ingredient_id 살아있음)
    }

    @Test
    @DisplayName("같은 ingredient + 같은 unit 두 번: 한 줄로 merge (amount/grams 합산)")
    void normalize_sameIngredientSameUnit_merge() {
        List<NormalizedLine> result = normalizer.normalize(
                List.of(
                        RawIngredientInput.of("마늘", "3", "쪽"),
                        RawIngredientInput.of("마늘", "5", "쪽")
                ),
                ingByName(), unitsByIng()
        );

        assertThat(result).hasSize(1);
        NormalizedLine merged = result.get(0);
        assertThat(merged.status()).isEqualTo(IngredientResolutionStatus.MAPPED);
        assertThat(merged.amountValue()).isEqualByComparingTo("8");           // 3 + 5
        assertThat(merged.normalizedGrams()).isEqualByComparingTo("40.000");  // 8 * 5g
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: merge 시 rawQuantityText도 합산값으로 갱신 (사용자 표시 일관성)")
    void normalize_merge_updatesRawQuantityText() {
        List<NormalizedLine> result = normalizer.normalize(
                List.of(
                        RawIngredientInput.of("마늘", "3", "쪽"),
                        RawIngredientInput.of("마늘", "5", "쪽")
                ),
                ingByName(), unitsByIng()
        );

        assertThat(result.get(0).rawQuantityText())
                .as("merge된 row의 raw 표시값은 합계 \"8\" — 첫 입력의 \"3\"이 남으면 회귀")
                .isEqualTo("8");
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: 첫 입력 정상 + 두 번째 정성수량(\"약간\") → 두 번째는 사라지지 않고 C' bypass PARTIAL로 보존")
    void normalize_secondInputIsSpecialQuantity_secondGoesToBypass() {
        // 마늘 3쪽 (MAPPED) + 마늘 약간 (정성 — 합산 불가). 두 번째 row가 사라지면 안 됨.
        List<NormalizedLine> result = normalizer.normalize(
                List.of(
                        RawIngredientInput.of("마늘", "3", "쪽"),
                        RawIngredientInput.of("마늘", "약간", "쪽")
                ),
                ingByName(), unitsByIng()
        );

        assertThat(result)
                .as("두 번째 row가 사라지면 안 된다 — pending에도 안 잡혀 데이터 손실 발생")
                .hasSize(2);

        NormalizedLine first = result.get(0);
        assertThat(first.status()).isEqualTo(IngredientResolutionStatus.MAPPED);
        assertThat(first.amountValue()).isEqualByComparingTo("3");
        assertThat(first.rawQuantityText())
                .as("두 번째와 합산되지 않아야 — \"3\" 그대로 유지")
                .isEqualTo("3");

        NormalizedLine second = result.get(1);
        assertThat(second.status()).isEqualTo(IngredientResolutionStatus.PARTIAL);
        assertThat(second.ingredientId())
                .as("UNIQUE 회피로 ingredient_id=null")
                .isNull();
        assertThat(second.ingredientUnitId())
                .as("unit_id 보존 (matched unit이므로)")
                .isEqualTo(10L);
        assertThat(second.rawQuantityText()).isEqualTo("약간");  // raw 보존
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: 첫 입력 정성수량(\"약간\") + 두 번째 정상 → 두 번째는 합산되지 않고 bypass")
    void normalize_firstInputIsSpecialQuantity_secondGoesToBypass() {
        List<NormalizedLine> result = normalizer.normalize(
                List.of(
                        RawIngredientInput.of("마늘", "약간", "쪽"),
                        RawIngredientInput.of("마늘", "3", "쪽")
                ),
                ingByName(), unitsByIng()
        );

        assertThat(result).hasSize(2);
        // 첫 row: MAPPED, amount=null
        assertThat(result.get(0).status()).isEqualTo(IngredientResolutionStatus.MAPPED);
        assertThat(result.get(0).amountValue()).isNull();
        // 두 번째 row: bypass (UNIQUE 회피 + 합산 불가)
        assertThat(result.get(1).status()).isEqualTo(IngredientResolutionStatus.PARTIAL);
        assertThat(result.get(1).ingredientId()).isNull();
        assertThat(result.get(1).amountValue()).isEqualByComparingTo("3");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: 같은 ingredient + 다른 unit → C' bypass (ingredient_id=null, unit_id 보존)")
    void normalize_sameIngredientDifferentUnit_bypass() {
        List<NormalizedLine> result = normalizer.normalize(
                List.of(
                        RawIngredientInput.of("마늘", "3", "쪽"),
                        RawIngredientInput.of("마늘", "1", "큰술")
                ),
                ingByName(), unitsByIng()
        );

        assertThat(result).hasSize(2);

        NormalizedLine first = result.get(0);
        assertThat(first.status()).isEqualTo(IngredientResolutionStatus.MAPPED);
        assertThat(first.ingredientId()).isEqualTo(1L);
        assertThat(first.ingredientUnitId()).isEqualTo(10L);

        NormalizedLine bypass = result.get(1);
        assertThat(bypass.status()).isEqualTo(IngredientResolutionStatus.PARTIAL);
        assertThat(bypass.ingredientId())
                .as("C' bypass: UNIQUE 회피로 ingredient_id=null이어야 한다")
                .isNull();
        assertThat(bypass.ingredientUnitId())
                .as("unit_id는 보존되어야 calc가 unit→ingredient로 복원 가능")
                .isEqualTo(11L);
        assertThat(bypass.unitOwnerIngredient())
                .as("calculator path 2를 위해 unit owner ingredient를 채워둔다")
                .isNotNull();
        assertThat(bypass.unitOwnerIngredient().getId()).isEqualTo(1L);
        assertThat(bypass.normalizedGrams()).isEqualByComparingTo("8.000");  // 1 * 8g
        assertThat(bypass.isBypassRow()).isTrue();
    }

    @Test
    @DisplayName("같은 ingredient — 첫 row가 PARTIAL(unit miss)이면, 두 번째도 bypass로 갈 수밖에 없음 (UNIQUE 보호)")
    void normalize_firstPartialThenMapped_secondBecomesBypass() {
        // 첫 row: 마늘 1봉지 (단위 매칭 실패) → PARTIAL with ingredient_id=1
        // 두 번째: 마늘 3쪽 (정상 MAPPED 후보) → 이미 ingredient_id=1이 점유됨 → bypass
        List<NormalizedLine> result = normalizer.normalize(
                List.of(
                        RawIngredientInput.of("마늘", "1", "봉지"),
                        RawIngredientInput.of("마늘", "3", "쪽")
                ),
                ingByName(), unitsByIng()
        );

        assertThat(result).hasSize(2);
        assertThat(result.get(0).status()).isEqualTo(IngredientResolutionStatus.PARTIAL);
        assertThat(result.get(0).ingredientId()).isEqualTo(1L);

        NormalizedLine second = result.get(1);
        assertThat(second.status()).isEqualTo(IngredientResolutionStatus.PARTIAL);
        assertThat(second.ingredientId()).isNull();
        assertThat(second.ingredientUnitId()).isEqualTo(10L);
        assertThat(second.isBypassRow()).isTrue();
    }

    @Test
    @DisplayName("customByUser=true: CUSTOM, 마스터 매칭 시도조차 안 함")
    void normalize_customByUser_alwaysCustom() {
        // 입력 이름이 마스터에 있어도 customByUser=true면 CUSTOM
        List<NormalizedLine> result = normalizer.normalize(
                List.of(RawIngredientInput.custom("마늘", "1", "쪽")),
                ingByName(), unitsByIng()
        );

        assertThat(result).hasSize(1);
        NormalizedLine line = result.get(0);
        assertThat(line.status()).isEqualTo(IngredientResolutionStatus.CUSTOM);
        assertThat(line.ingredientId()).isNull();
        assertThat(line.ingredientUnitId()).isNull();
        assertThat(line.customByUser()).isTrue();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: AI/YouTube path는 customByUser=false 강제 → 매칭 실패해도 UNRESOLVED이지 CUSTOM 아님")
    void normalize_aiPath_neverProducesCustom() {
        // RawIngredientInput.of(...)는 customByUser=false (AI/YouTube path 시뮬레이션)
        List<NormalizedLine> result = normalizer.normalize(
                List.of(RawIngredientInput.of("황태머리", "1", "마리")),
                ingByName(), unitsByIng()
        );

        assertThat(result.get(0).status())
                .as("AI/YouTube가 못 푼 재료는 UNRESOLVED여야 — CUSTOM은 사용자 명시 의도일 때만")
                .isEqualTo(IngredientResolutionStatus.UNRESOLVED);
    }

    @Test
    @DisplayName("\"약간\" 같은 정성 수량: amount=null이지만 normalizer는 raw를 보존하고 status는 MAPPED 유지")
    void normalize_specialQuantity_preservesRaw() {
        // 마늘 약간 — unit이 매칭 안 됨 ("약간"이 unit 자리에 없으므로 unit 자체가 빈 케이스)
        List<NormalizedLine> result = normalizer.normalize(
                List.of(RawIngredientInput.of("마늘", "약간", "쪽")),
                ingByName(), unitsByIng()
        );

        NormalizedLine line = result.get(0);
        // ingredient hit + unit hit이지만 amount 없음 → MAPPED인데 grams 계산 불가
        assertThat(line.status()).isEqualTo(IngredientResolutionStatus.MAPPED);
        assertThat(line.amountValue()).isNull();
        assertThat(line.normalizedGrams())
                .as("amount 없으면 grams 계산 안 됨 (calculator가 보류 처리)")
                .isNull();
        assertThat(line.rawQuantityText()).isEqualTo("약간");  // raw 보존
    }

    @Test
    @DisplayName("빈 입력 / null 입력: 빈 결과")
    void normalize_emptyOrNullInputs() {
        assertThat(normalizer.normalize(null, ingByName(), unitsByIng())).isEmpty();
        assertThat(normalizer.normalize(List.of(), ingByName(), unitsByIng())).isEmpty();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: canonicalizeName이 공백 변형을 흡수 (\"청 양 고추\" → \"청양고추\" key)")
    void normalize_whitespaceVariantInName_matches() {
        Ingredient pepper = Ingredient.builder().id(3L).name("청양고추").build();
        IngredientUnit pieces = unit(30L, pepper, "개", "10");

        Map<String, Ingredient> map = Map.of(RecipeIngredientNormalizer.canonicalizeName("청양고추"), pepper);
        Map<Long, List<IngredientUnit>> units = Map.of(3L, List.of(pieces));

        // 자연어 변형: 공백 들어간 입력
        List<NormalizedLine> r1 = normalizer.normalize(
                List.of(RawIngredientInput.of("청 양 고추", "1", "개")), map, units);
        List<NormalizedLine> r2 = normalizer.normalize(
                List.of(RawIngredientInput.of("청양 고추", "1", "개")), map, units);

        assertThat(r1.get(0).status()).isEqualTo(IngredientResolutionStatus.MAPPED);
        assertThat(r1.get(0).ingredientId()).isEqualTo(3L);
        assertThat(r2.get(0).status()).isEqualTo(IngredientResolutionStatus.MAPPED);
    }

    @Test
    @DisplayName("canonicalizeName 직접: trim + 공백 모두 제거 + lowercase")
    void canonicalizeName_strips_whitespace_and_lowercases() {
        assertThat(RecipeIngredientNormalizer.canonicalizeName("청 양 고추")).isEqualTo("청양고추");
        assertThat(RecipeIngredientNormalizer.canonicalizeName("  ABC ")).isEqualTo("abc");
        assertThat(RecipeIngredientNormalizer.canonicalizeName("Onion Ring"))
                .isEqualTo("onionring");
        assertThat(RecipeIngredientNormalizer.canonicalizeName(null)).isEmpty();
        assertThat(RecipeIngredientNormalizer.canonicalizeName("   ")).isEmpty();
    }

    @Test
    @DisplayName("단위 영문 alias: 1 Tbsp가 마늘 큰술과 매칭된다 (UnitNormalizer 통합)")
    void normalize_englishUnitAlias_resolves() {
        List<NormalizedLine> result = normalizer.normalize(
                List.of(RawIngredientInput.of("마늘", "1", "Tbsp")),
                ingByName(), unitsByIng()
        );

        NormalizedLine line = result.get(0);
        assertThat(line.status()).isEqualTo(IngredientResolutionStatus.MAPPED);
        assertThat(line.ingredientUnitId()).isEqualTo(11L);  // garlicSpoon
    }

    @Test
    @DisplayName("Calculator 통합: bypass row를 calculator로 넘기면 unit_owner 경로로 계산된다")
    void normalize_thenCalculate_bypassPath() {
        // 마늘에 per-g 7개 모두 채움 (calculator가 전부 요구)
        Ingredient garlicWithPerG = Ingredient.builder()
                .id(1L).name("마늘")
                .kcalPerG(new BigDecimal("1.5"))
                .pricePerG(new BigDecimal("10"))
                .carbohydrateGPerG(new BigDecimal("0.3"))
                .proteinGPerG(new BigDecimal("0.06"))
                .fatGPerG(new BigDecimal("0.005"))
                .sugarGPerG(new BigDecimal("0.01"))
                .sodiumMgPerG(new BigDecimal("0.02"))
                .build();
        IngredientUnit gp = unit(10L, garlicWithPerG, "쪽", "5");
        IngredientUnit gs = unit(11L, garlicWithPerG, "큰술", "8");

        List<NormalizedLine> normalized = normalizer.normalize(
                List.of(
                        RawIngredientInput.of("마늘", "3", "쪽"),
                        RawIngredientInput.of("마늘", "1", "큰술")
                ),
                Map.of("마늘", garlicWithPerG),
                Map.of(1L, List.of(gp, gs))
        );

        // bypass 라인을 calculator input으로 변환
        NormalizedLine bypass = normalized.get(1);
        // **SHOULD 회귀 차단**: bypass row의 unitOwnerIngredient가 LAZY load 없이 이미 채워져 있어야 한다.
        assertThat(bypass.unitOwnerIngredient())
                .as("normalizer가 lookup된 ingredient를 직접 채워줘야 N+1 회피")
                .isSameAs(garlicWithPerG);

        RecipeIngredientCalculator calculator = new RecipeIngredientCalculator();
        RecipeIngredientCalculator.LineCalculation calc = calculator.calculate(
                new RecipeIngredientCalculator.CalculationLineInput(
                        bypass.status(),
                        bypass.normalizedGrams(),
                        bypass.resolvedIngredient(),     // null
                        bypass.unitOwnerIngredient(), null,    // garlicWithPerG via lookup (not LAZY)
                        null, null, null, null, null, null, null
                )
        );

        assertThat(calc.included()).isTrue();
        assertThat(calc.calories()).isEqualByComparingTo("12.000");  // 8g * 1.5
        assertThat(calc.price()).isEqualTo(80L);                      // 8g * 10
    }
}
