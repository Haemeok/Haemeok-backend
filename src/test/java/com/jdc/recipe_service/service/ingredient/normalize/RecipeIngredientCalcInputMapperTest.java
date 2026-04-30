package com.jdc.recipe_service.service.ingredient.normalize;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientCalculator.CalculationLineInput;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * RecipeIngredient → CalculationLineInput 변환 mapper 단위 테스트.
 *
 * <p>이 mapper는 dev detail read와 aggregate 백필이 공유한다 — 두 곳이 어긋나면 detail 화면 합산값과
 * 목록 검색이 다른 값을 보일 수 있다.
 */
class RecipeIngredientCalcInputMapperTest {

    private static Ingredient master(Long id, String name) {
        return Ingredient.builder().id(id).name(name).build();
    }

    @Test
    @DisplayName("MAPPED row: ingredient + normalizedGrams + status가 그대로 input에 매핑")
    void mapped_normalCase() {
        Ingredient garlic = master(1L, "마늘");
        RecipeIngredient ri = RecipeIngredient.builder()
                .id(101L).ingredient(garlic)
                .rawName("마늘").rawQuantityText("3").rawUnitText("쪽")
                .ingredientUnitId(10L)
                .normalizedGrams(new BigDecimal("15"))
                .resolutionStatus("MAPPED")
                .build();

        CalculationLineInput input = RecipeIngredientCalcInputMapper.toInput(ri, Map.of());

        assertThat(input.status()).isEqualTo(IngredientResolutionStatus.MAPPED);
        assertThat(input.canonicalIngredient()).isSameAs(garlic);
        assertThat(input.normalizedGrams()).isEqualByComparingTo("15");
        assertThat(input.unitOwnerIngredient()).isNull();  // unitsById가 비어 있음
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: C' bypass row(ingredient_id=null + unit_id 보존)는 unitsById에서 unitOwner 복원")
    void bypassRow_resolvesUnitOwnerFromMap() {
        Ingredient garlic = master(1L, "마늘");
        IngredientUnit garlicSpoon = IngredientUnit.builder()
                .id(11L).ingredient(garlic).unitLabelKo("큰술")
                .gramsPerUnit(new BigDecimal("8"))
                .edibleGramsPerUnit(new BigDecimal("8"))
                .build();
        RecipeIngredient bypass = RecipeIngredient.builder()
                .id(102L).ingredient(null)
                .rawName("마늘").rawUnitText("큰술")
                .customName("마늘").customUnit("큰술")
                .ingredientUnitId(11L)
                .normalizedGrams(new BigDecimal("8"))
                .resolutionStatus("PARTIAL")
                .build();

        CalculationLineInput input = RecipeIngredientCalcInputMapper.toInput(bypass, Map.of(11L, garlicSpoon));

        assertThat(input.status()).isEqualTo(IngredientResolutionStatus.PARTIAL);
        assertThat(input.canonicalIngredient()).isNull();
        assertThat(input.unitOwnerIngredient())
                .as("unit→ingredient 복원으로 calculator path 2 작동 가능")
                .isSameAs(garlic);
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: candidate id 없는 legacy row의 custom* 0/ZERO는 null로 보호 (pending 처리)")
    void legacyZeroDefaults_convertedToNull() {
        Ingredient master = master(1L, "재료");
        RecipeIngredient legacy = RecipeIngredient.builder()
                .id(101L).ingredient(master)
                .resolutionStatus("MAPPED")
                .customCalorie(BigDecimal.ZERO)
                .customPrice(0)
                .customCarbohydrate(BigDecimal.ZERO)
                .customProtein(BigDecimal.ZERO)
                .customFat(BigDecimal.ZERO)
                .customSugar(BigDecimal.ZERO)
                .customSodium(BigDecimal.ZERO)
                // ingredientCandidateId 없음 (null)
                .build();

        CalculationLineInput input = RecipeIngredientCalcInputMapper.toInput(legacy, Map.of());

        assertThat(input.ingredientCandidateId()).isNull();
        assertThat(input.customCalorie()).as("0은 누락 가능성 → null로 변환").isNull();
        assertThat(input.customPrice()).isNull();
        assertThat(input.customCarbohydrate()).isNull();
        assertThat(input.customSodium()).isNull();
    }

    @Test
    @DisplayName("**MUST 회귀 차단**: candidate id 박힌 row의 custom* 0은 정상 명시값으로 신뢰 (그대로 전달)")
    void aiFallbackZeros_preserved() {
        RecipeIngredient aiRow = RecipeIngredient.builder()
                .id(201L).ingredient(null)
                .rawName("황태머리").customName("황태머리")
                .resolutionStatus("UNRESOLVED")
                .ingredientCandidateId(5001L)        // ★ AI fallback signal
                .customCalorie(new BigDecimal("80"))
                .customPrice(500)
                .customCarbohydrate(new BigDecimal("12"))
                .customProtein(new BigDecimal("3"))
                .customFat(BigDecimal.ZERO)            // ★ 정상 명시 0
                .customSugar(BigDecimal.ZERO)          // ★ 정상 명시 0
                .customSodium(new BigDecimal("200"))
                .build();

        CalculationLineInput input = RecipeIngredientCalcInputMapper.toInput(aiRow, Map.of());

        assertThat(input.ingredientCandidateId()).isEqualTo(5001L);
        assertThat(input.customCalorie()).isEqualByComparingTo("80");
        assertThat(input.customPrice()).isEqualTo(500);
        assertThat(input.customFat())
                .as("AI가 명시한 0은 null로 변환되지 않고 그대로 전달")
                .isEqualByComparingTo("0");
        assertThat(input.customSugar()).isEqualByComparingTo("0");
    }

    @Test
    @DisplayName("parseStatus: dirty status (\" partial \", \"PARTIAL\") trim+upper 정규화")
    void parseStatus_dirtyValueTolerant() {
        RecipeIngredient ri = RecipeIngredient.builder()
                .id(101L).ingredient(master(1L, "재료"))
                .ingredientUnitId(10L)
                .resolutionStatus(" partial ")
                .build();

        IngredientResolutionStatus s = RecipeIngredientCalcInputMapper.parseStatus(ri);
        assertThat(s).isEqualTo(IngredientResolutionStatus.PARTIAL);
    }

    @Test
    @DisplayName("parseStatus: status null + ingredient_id=null + unit_id 살아있음 → PARTIAL (C' bypass 추정)")
    void parseStatus_nullStatusBypassRow_inferredAsPartial() {
        RecipeIngredient ri = RecipeIngredient.builder()
                .id(101L).ingredient(null)
                .ingredientUnitId(11L)
                .customName("마늘")
                .resolutionStatus(null)
                .build();

        IngredientResolutionStatus s = RecipeIngredientCalcInputMapper.parseStatus(ri);
        assertThat(s)
                .as("ingredient_id=null이라도 unit_id 살아있으면 PARTIAL — calculator path 2 진입 가드")
                .isEqualTo(IngredientResolutionStatus.PARTIAL);
    }

    @Test
    @DisplayName("parseStatus: status null + ingredient=null + unit_id=null + customName 있음 → CUSTOM")
    void parseStatus_nullStatusCustomRow_inferredAsCustom() {
        RecipeIngredient ri = RecipeIngredient.builder()
                .id(101L).ingredient(null)
                .customName("엄마표양념")
                .resolutionStatus(null)
                .build();

        assertThat(RecipeIngredientCalcInputMapper.parseStatus(ri))
                .isEqualTo(IngredientResolutionStatus.CUSTOM);
    }
}
