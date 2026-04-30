package com.jdc.recipe_service.service.ingredient.normalize;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.CalculationLineInput;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.CalculationSummary;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.LineCalculation;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class RecipeIngredientCalculatorTest {

    private final RecipeIngredientCalculator calculator = new RecipeIngredientCalculator();

    /** 양파 1g당: kcal 0.4, 가격 5원, 탄수 0.09g, 단백 0.011g 등 — 임의지만 일관된 값. */
    private static Ingredient onionWithPerGram() {
        return Ingredient.builder()
                .id(100L).name("양파")
                .kcalPerG(new BigDecimal("0.400000"))
                .pricePerG(new BigDecimal("5.0000"))
                .carbohydrateGPerG(new BigDecimal("0.090000"))
                .proteinGPerG(new BigDecimal("0.011000"))
                .fatGPerG(new BigDecimal("0.001000"))
                .sugarGPerG(new BigDecimal("0.042000"))
                .sodiumMgPerG(new BigDecimal("0.040000"))
                .build();
    }

    /** per-g 값이 전부 비어있는 마스터 — 백필 미완 시뮬레이션 */
    private static Ingredient emptyPerGramIngredient() {
        return Ingredient.builder().id(200L).name("미백필").build();
    }

    @Test
    @DisplayName("Path 1: MAPPED + normalizedGrams + per-g → grams × per-g 계산")
    void mapped_withPerGram_calculatesNutritionAndPrice() {
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.MAPPED,
                new BigDecimal("100"),
                onionWithPerGram(),
                null, null,
                null, null, null, null, null, null, null
        );

        LineCalculation calc = calculator.calculate(line);

        assertThat(calc.included()).isTrue();
        assertThat(calc.calories()).isEqualByComparingTo("40.000");  // 0.4 * 100
        assertThat(calc.price()).isEqualTo(500L);                     // 5 * 100
        assertThat(calc.carbohydrate()).isEqualByComparingTo("9.000");
        assertThat(calc.protein()).isEqualByComparingTo("1.100");
        assertThat(calc.sodium()).isEqualByComparingTo("4.000");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: PARTIAL bypass — ingredient_id=null이어도 unitOwnerIngredient로 계산된다 (C' 설계 잠금)")
    void partialBypass_calculatesViaUnitOwner() {
        Ingredient onion = onionWithPerGram();
        // C' bypass: canonicalIngredient는 null (ingredient_id=null), unitOwnerIngredient는 채워짐
        CalculationLineInput bypass = new CalculationLineInput(
                IngredientResolutionStatus.PARTIAL,
                new BigDecimal("50"),
                null,           // ingredient_id 없음 — UNIQUE 회피용
                onion, null,          // unit→ingredient 경로 복원
                null, null, null, null, null, null, null
        );

        LineCalculation calc = calculator.calculate(bypass);

        assertThat(calc.included())
                .as("C' bypass row는 포함되어야 한다 — 0으로 삼키면 설계가 깨진다")
                .isTrue();
        assertThat(calc.calories()).isEqualByComparingTo("20.000");  // 0.4 * 50
        assertThat(calc.price()).isEqualTo(250L);
    }

    @Test
    @DisplayName("Path 3: 명시 customCalorie/customPrice → 그 값을 그대로 사용")
    void custom_explicitValues_usedDirectly() {
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.CUSTOM,
                null, null, null, null,
                new BigDecimal("123.5"),
                7000,
                new BigDecimal("10"),
                new BigDecimal("20"),
                new BigDecimal("5"),
                new BigDecimal("2"),
                new BigDecimal("100")
        );

        LineCalculation calc = calculator.calculate(line);

        assertThat(calc.included()).isTrue();
        assertThat(calc.calories()).isEqualByComparingTo("123.5");
        assertThat(calc.price()).isEqualTo(7000L);
        assertThat(calc.carbohydrate()).isEqualByComparingTo("10");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: PARTIAL인데 unitOwner도 없고 명시값도 없으면 pending (0으로 삼키지 않음)")
    void partial_noUnitOwnerNoExplicit_isPending() {
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.PARTIAL,
                null, null, null, null,
                null, null, null, null, null, null, null
        );

        LineCalculation calc = calculator.calculate(line);

        assertThat(calc.included())
                .as("계산 불가는 pending이지 0이 아니다")
                .isFalse();
    }

    @Test
    @DisplayName("MAPPED인데 normalizedGrams=null이면 pending")
    void mapped_withoutGrams_isPending() {
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.MAPPED,
                null,                       // grams 없음
                onionWithPerGram(),
                null, null,
                null, null, null, null, null, null, null
        );

        assertThat(calculator.calculate(line).included()).isFalse();
    }

    @Test
    @DisplayName("MAPPED + grams 있지만 ingredient의 per-g 값이 모두 null이면 pending (백필 미완)")
    void mapped_emptyPerGram_isPending() {
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.MAPPED,
                new BigDecimal("100"),
                emptyPerGramIngredient(),
                null, null,
                null, null, null, null, null, null, null
        );

        assertThat(calculator.calculate(line).included()).isFalse();
    }

    @Test
    @DisplayName("UNRESOLVED + 명시값 없음: pending")
    void unresolved_withoutExplicit_isPending() {
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.UNRESOLVED,
                null, null, null, null,
                null, null, null, null, null, null, null
        );

        assertThat(calculator.calculate(line).included()).isFalse();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: customPrice만 있고 customCalorie 없음 → pending (한쪽만 명시는 0으로 흘려보내지 않음)")
    void unresolved_withCustomPriceOnly_isPending() {
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.UNRESOLVED,
                null, null, null, null,
                null,
                3000,        // customPrice만 명시 (customCalorie 누락)
                null, null, null, null, null
        );

        LineCalculation calc = calculator.calculate(line);
        assertThat(calc.included())
                .as("kcal+price 둘 다 명시했을 때만 included")
                .isFalse();
    }

    @Test
    @DisplayName("UNRESOLVED + ingredient_candidate_id + 7개 custom 필드 모두 명시: path 3 included")
    void unresolved_withCandidateAndAllCustomFields_pathThree() {
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.UNRESOLVED,
                null, null, null,
                /* ingredientCandidateId= */ 5001L,  // INGREDIENT candidate가 fallback signal
                new BigDecimal("80"),
                3000,
                new BigDecimal("12.0"),  // 탄
                new BigDecimal("3.0"),   // 단
                new BigDecimal("1.0"),   // 지
                new BigDecimal("4.0"),   // 당
                new BigDecimal("200")    // 나
        );

        LineCalculation calc = calculator.calculate(line);
        assertThat(calc.included())
                .as("candidate signal + 7개 custom 필드 모두 → path 3 included")
                .isTrue();
        assertThat(calc.calories()).isEqualByComparingTo("80");
        assertThat(calc.price()).isEqualTo(3000L);
        assertThat(calc.carbohydrate()).isEqualByComparingTo("12.0");
        assertThat(calc.sodium()).isEqualByComparingTo("200");
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: AI fallback (candidate id) + customCalorie/customPrice만 있고 macros 누락 → pending (체계적 합산 왜곡 방어)")
    void aiFallback_macrosMissing_isPending() {
        // AI가 line-total을 atomic하게 추정해야 하는데 calorie/price만 보내고 macros 누락 시,
        // 0으로 합산하면 추이/통계가 체계적으로 낮게 나옴 — pending으로 두는 게 더 정확.
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.UNRESOLVED,
                null, null, null,
                /* ingredientCandidateId= */ 5001L,  // signal은 있음
                new BigDecimal("80"),
                3000,
                null, null, null, null, null  // macros 5개 모두 누락
        );

        LineCalculation calc = calculator.calculate(line);
        assertThat(calc.included())
                .as("AI fallback인데 macros 누락 → pending (0으로 흘려보내지 않음)")
                .isFalse();
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: AI fallback (candidate id) + 7개 중 1개라도 누락 → pending (방어선)")
    void aiFallback_oneMacroMissing_isPending() {
        // 단백질 한 필드만 누락해도 pending — 부분 누락 허용하면 모델이 일관되게 채우지 않게 됨.
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.UNRESOLVED,
                null, null, null,
                /* ingredientCandidateId= */ 5001L,
                new BigDecimal("80"),
                3000,
                new BigDecimal("12.0"),
                null,  // protein 누락
                new BigDecimal("1.0"),
                new BigDecimal("4.0"),
                new BigDecimal("200")
        );

        assertThat(calculator.calculate(line).included())
                .as("AI fallback에서 한 필드라도 null이면 pending")
                .isFalse();
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: AI fallback (candidate id) + customFat=0/customSugar=0 정상 명시값 → included (0과 null의 의미가 다름)")
    void aiFallback_macrosWithExplicitZeros_stillIncluded() {
        // AI가 line-total로 "지방 0g, 당 0g"을 정상 명시한 케이스. 0은 명시값이라 included.
        // 이 invariant가 깨지면 read 단의 nonZeroOrNull(0→null) 변환에 막혀 부분 누락 처리됨.
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.UNRESOLVED,
                null, null, null,
                /* ingredientCandidateId= */ 5001L,
                new BigDecimal("80"),
                3000,
                new BigDecimal("12.0"),
                new BigDecimal("3.0"),
                BigDecimal.ZERO,           // fat=0 정상 명시값
                BigDecimal.ZERO,           // sugar=0 정상 명시값
                new BigDecimal("200")
        );

        LineCalculation calc = calculator.calculate(line);
        assertThat(calc.included())
                .as("AI가 0을 정상 line-total로 명시한 macros는 included — null과 0은 의미가 다름")
                .isTrue();
        assertThat(calc.calories()).isEqualByComparingTo("80");
        assertThat(calc.fat()).isEqualByComparingTo("0");
        assertThat(calc.sugar()).isEqualByComparingTo("0");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: user CUSTOM (status=CUSTOM)은 macros 없어도 calorie+price만 있으면 included (legacy 입력 호환)")
    void userCustom_macrosMissing_stillIncluded() {
        // 사용자 직접 입력은 macros 입력이 번거로워 V1부터 calorie+price만으로 통과시키는 정책 유지.
        // AI fallback과 달리 사용자는 누락된 macros를 0으로 받아들이는 의도일 수 있음.
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.CUSTOM,
                null, null, null,
                null,  // candidate 없음 — status=CUSTOM signal로 path 3
                new BigDecimal("80"),
                3000,
                null, null, null, null, null  // macros 누락
        );

        LineCalculation calc = calculator.calculate(line);
        assertThat(calc.included())
                .as("user CUSTOM은 macros optional — calorie+price만으로 included")
                .isTrue();
        assertThat(calc.calories()).isEqualByComparingTo("80");
        assertThat(calc.carbohydrate()).isEqualByComparingTo("0");
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: candidate signal 없는 customPrice/customCalorie는 pending (legacy default 0 보호)")
    void unresolved_withCustomValuesButNoCandidateOrCustomFlag_isPending() {
        // 기존 row의 customCalorie=BigDecimal.ZERO, customPrice=0이 default라 그냥 통과시키면 0원으로 흘러감.
        // signal (ingredient_candidate_id 또는 status=CUSTOM) 없으면 path 3 진입 차단.
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.UNRESOLVED,
                null, null, null,
                /* ingredientCandidateId= */ null,  // ← signal 없음
                new BigDecimal("80"),
                3000,
                null, null, null, null, null
        );

        LineCalculation calc = calculator.calculate(line);
        assertThat(calc.included())
                .as("UNRESOLVED + candidate=null + status≠CUSTOM이면 custom* 무시하고 pending")
                .isFalse();
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: canonical 가능하면 candidate 있어도 path 1 우선 (custom* 무시)")
    void mapped_withCandidateAndCustomValues_canonicalWins() {
        // canonical row(MAPPED + per-g 가능)에 AI가 잘못 custom*를 줬어도 canonical이 우선되어야.
        // 후속 unit/ingredient seed로 path 1 자동 활성화 시 자연스럽게 custom*는 무시되는 invariant.
        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.MAPPED,
                new BigDecimal("100"),
                onionWithPerGram(),
                null,
                /* ingredientCandidateId= */ 5001L,  // 실수로 candidate 박힘
                new BigDecimal("9999"),               // AI가 추정한 잘못된 custom*
                9999,
                null, null, null, null, null
        );

        LineCalculation calc = calculator.calculate(line);
        assertThat(calc.included()).isTrue();
        assertThat(calc.calories())
                .as("canonical(40 = 0.4 × 100)이 path 3(9999)보다 우선")
                .isEqualByComparingTo("40.000");
        assertThat(calc.price())
                .as("canonical 가격이 우선")
                .isEqualTo(500L);
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: per-g 7개 필드 중 일부만 채워진 마스터는 pending (하나라도 null이면 included 아님)")
    void mapped_partialPerGram_isPending() {
        // kcal만 있고 나머지 per-g 누락 — 백필 미완 시뮬레이션
        Ingredient partialMaster = Ingredient.builder()
                .id(300L).name("부분백필")
                .kcalPerG(new BigDecimal("1.0"))
                .pricePerG(new BigDecimal("10"))
                // carbohydrate/protein/fat/sugar/sodium per-g 누락
                .build();

        CalculationLineInput line = new CalculationLineInput(
                IngredientResolutionStatus.MAPPED,
                new BigDecimal("100"),
                partialMaster,
                null, null,
                null, null, null, null, null, null, null
        );

        assertThat(calculator.calculate(line).included())
                .as("per-g 7개 중 하나라도 null이면 0으로 흘리지 않고 pending")
                .isFalse();
    }

    @Test
    @DisplayName("Summary: 4 status counters + calculated/pending 분리")
    void summary_countsByStatus() {
        Ingredient onion = onionWithPerGram();

        List<CalculationLineInput> lines = List.of(
                // MAPPED 2개 (둘 다 계산 포함)
                new CalculationLineInput(IngredientResolutionStatus.MAPPED, new BigDecimal("100"), onion, null, null, null, null, null, null, null, null, null),
                new CalculationLineInput(IngredientResolutionStatus.MAPPED, new BigDecimal("50"), onion, null, null, null, null, null, null, null, null, null),
                // PARTIAL bypass 1개 (계산 포함)
                new CalculationLineInput(IngredientResolutionStatus.PARTIAL, new BigDecimal("20"), null, onion, null, null, null, null, null, null, null, null),
                // PARTIAL pending 1개 (계산 불가)
                new CalculationLineInput(IngredientResolutionStatus.PARTIAL, null, null, null, null, null, null, null, null, null, null, null),
                // UNRESOLVED 1개 (계산 불가)
                new CalculationLineInput(IngredientResolutionStatus.UNRESOLVED, null, null, null, null, null, null, null, null, null, null, null),
                // CUSTOM 1개 with 명시값
                new CalculationLineInput(IngredientResolutionStatus.CUSTOM, null, null, null, null, new BigDecimal("100"), 5000, null, null, null, null, null)
        );

        CalculationSummary summary = calculator.summarize(lines);

        assertThat(summary.mappedCount()).isEqualTo(2);
        assertThat(summary.partialCount()).isEqualTo(2);
        assertThat(summary.unresolvedCount()).isEqualTo(1);
        assertThat(summary.customCount()).isEqualTo(1);
        assertThat(summary.calculatedCount()).isEqualTo(4);  // mapped 2 + bypass 1 + custom 1
        assertThat(summary.pendingCount()).isEqualTo(2);     // partial without unit + unresolved
        // total kcal = 100*0.4 + 50*0.4 + 20*0.4 + 100 (custom) = 40+20+8+100 = 168
        assertThat(summary.totalCalories()).isEqualByComparingTo("168.000");
        // total price = 500 + 250 + 100 + 5000 = 5850
        assertThat(summary.totalIngredientCost()).isEqualTo(5850L);
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: pending row는 합계에 더해지지 않는다 (0이 아닌 미반영)")
    void summary_pendingNotAddedAsZero() {
        Ingredient onion = onionWithPerGram();

        List<CalculationLineInput> linesWithPending = List.of(
                new CalculationLineInput(IngredientResolutionStatus.MAPPED, new BigDecimal("100"), onion, null, null, null, null, null, null, null, null, null),
                new CalculationLineInput(IngredientResolutionStatus.UNRESOLVED, null, null, null, null, null, null, null, null, null, null, null)
        );

        CalculationSummary summary = calculator.summarize(linesWithPending);

        // 양파 100g 한 줄만 합산
        assertThat(summary.totalCalories()).isEqualByComparingTo("40.000");
        assertThat(summary.calculatedCount()).isEqualTo(1);
        assertThat(summary.pendingCount()).isEqualTo(1);
    }

    @Test
    @DisplayName("null line / 빈 list / null list 모두 안전하게 처리")
    void summary_nullSafe() {
        assertThat(calculator.summarize(null).calculatedCount()).isEqualTo(0);
        assertThat(calculator.summarize(List.of()).pendingCount()).isEqualTo(0);
    }
}
