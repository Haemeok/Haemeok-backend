package com.jdc.recipe_service.service.ingredient.normalize;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

class IngredientUnitResolverTest {

    private final IngredientUnitResolver resolver = new IngredientUnitResolver();

    private static IngredientUnit unit(Long id, Ingredient ingredient, String normalizedLabel) {
        return IngredientUnit.builder()
                .id(id)
                .ingredient(ingredient)
                .unitLabelKo(normalizedLabel)
                .normalizedUnitLabel(normalizedLabel)
                .gramsPerUnit(new BigDecimal("5"))
                .edibleGramsPerUnit(new BigDecimal("5"))
                .isDefault(false)
                .build();
    }

    @Test
    @DisplayName("matching label: 해당 IngredientUnit 반환")
    void resolve_matchingLabel_returnsUnit() {
        Ingredient garlic = Ingredient.builder().id(1L).name("마늘").build();
        IngredientUnit pieces = unit(10L, garlic, "쪽");
        IngredientUnit grams = unit(11L, garlic, "g");

        Optional<IngredientUnit> result = resolver.resolve("쪽", List.of(pieces, grams));

        assertThat(result).isPresent();
        assertThat(result.get().getId()).isEqualTo(10L);
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: candidate의 ingredient가 null이어도 label만으로 resolve된다 (LAZY 차단 invariant)")
    void resolve_candidateWithNullIngredient_stillMatchesByLabel() {
        // unit.getIngredient()를 호출하지 않는다는 invariant 잠금. JPA LAZY proxy 시뮬레이션:
        // 후보의 ingredient association이 비어 있어도 resolver는 label만 본다.
        IngredientUnit detachedUnit = IngredientUnit.builder()
                .id(99L)
                .ingredient(null)                // LAZY association이 손대지 않은 상태
                .unitLabelKo("쪽")
                .normalizedUnitLabel("쪽")
                .gramsPerUnit(new BigDecimal("5"))
                .edibleGramsPerUnit(new BigDecimal("5"))
                .isDefault(false)
                .build();

        Optional<IngredientUnit> result = resolver.resolve("쪽", List.of(detachedUnit));

        assertThat(result).isPresent();
        assertThat(result.get().getId()).isEqualTo(99L);
    }

    @Test
    @DisplayName("label이 candidates에 없으면 empty")
    void resolve_labelNotInCandidates_returnsEmpty() {
        IngredientUnit grams = unit(11L, Ingredient.builder().id(1L).name("마늘").build(), "g");
        assertThat(resolver.resolve("큰술", List.of(grams))).isEmpty();
    }

    @Test
    @DisplayName("normalizedUnitLabel blank/null → empty")
    void resolve_blankLabel_returnsEmpty() {
        IngredientUnit pieces = unit(10L, Ingredient.builder().id(1L).name("마늘").build(), "쪽");
        assertThat(resolver.resolve("", List.of(pieces))).isEmpty();
        assertThat(resolver.resolve("   ", List.of(pieces))).isEmpty();
        assertThat(resolver.resolve(null, List.of(pieces))).isEmpty();
    }

    @Test
    @DisplayName("candidates 비거나 null → empty")
    void resolve_emptyCandidates_returnsEmpty() {
        assertThat(resolver.resolve("쪽", List.of())).isEmpty();
        assertThat(resolver.resolve("쪽", null)).isEmpty();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: 첫 매칭만 반환 (안정 동작 보장)")
    void resolve_returnsFirstMatch() {
        Ingredient garlic = Ingredient.builder().id(1L).name("마늘").build();
        IngredientUnit firstMatch = unit(10L, garlic, "쪽");
        IngredientUnit duplicate = unit(99L, garlic, "쪽"); // 데이터 이상 시 방어

        Optional<IngredientUnit> result = resolver.resolve("쪽", List.of(firstMatch, duplicate));

        assertThat(result).isPresent();
        assertThat(result.get().getId()).isEqualTo(10L);
    }

    @Test
    @DisplayName("caller 책임 분리: 잘못된 ingredient의 unit이 candidate에 섞여 있으면 그대로 매칭됨 (caller가 pre-filter 안 했음을 드러냄)")
    void resolve_callerMustPreFilter() {
        // 새 contract: ingredient 필터링은 caller (unitsByIngredientId.get(id)) 책임.
        // resolver는 누가 줬는지 물어보지 않고 label만 본다.
        Ingredient onion = Ingredient.builder().id(2L).name("양파").build();
        IngredientUnit onionPiece = unit(20L, onion, "쪽");

        // 마늘 후보 자리에 양파 unit을 잘못 넣은 경우 — resolver는 ingredient를 모르므로 그대로 매칭
        Optional<IngredientUnit> result = resolver.resolve("쪽", List.of(onionPiece));

        assertThat(result)
                .as("ingredient pre-filter 책임은 caller에 있다")
                .isPresent();
    }
}
