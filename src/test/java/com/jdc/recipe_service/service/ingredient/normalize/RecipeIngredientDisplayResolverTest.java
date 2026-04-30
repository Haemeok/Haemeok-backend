package com.jdc.recipe_service.service.ingredient.normalize;

import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientDisplayResolver.DisplayLine;
import com.jdc.recipe_service.service.ingredient.normalize.RecipeIngredientDisplayResolver.LineDisplayInput;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class RecipeIngredientDisplayResolverTest {

    private final RecipeIngredientDisplayResolver resolver = new RecipeIngredientDisplayResolver();

    @Test
    @DisplayName("raw 모두 채워짐: rawName/rawQuantityText/rawUnitText 우선")
    void resolve_rawFilled_winsAll() {
        LineDisplayInput in = new LineDisplayInput(
                "마늘", "마늘C", "마늘M",
                "3", "999",
                "쪽", "큰술", "g"
        );

        DisplayLine out = resolver.resolve(in);

        assertThat(out.name()).isEqualTo("마늘");
        assertThat(out.quantity()).isEqualTo("3");
        assertThat(out.unit()).isEqualTo("쪽");
    }

    @Test
    @DisplayName("rawName 비어있고 customName 있으면 customName")
    void resolve_blankRawName_fallsBackToCustom() {
        LineDisplayInput in = new LineDisplayInput(
                null, "엄마표양념", "그냥된장",
                null, null,
                null, null, null
        );
        assertThat(resolver.resolve(in).name()).isEqualTo("엄마표양념");
    }

    @Test
    @DisplayName("rawName/customName 모두 비어있으면 ingredientName 사용")
    void resolve_blankRawAndCustom_fallsBackToIngredient() {
        LineDisplayInput in = new LineDisplayInput(
                "  ", "", "마늘",
                null, null,
                null, null, null
        );
        assertThat(resolver.resolve(in).name()).isEqualTo("마늘");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: 우선순위는 raw > custom > ingredient (역전 시 깨짐)")
    void resolve_priorityOrder() {
        // raw만 있으면 raw
        assertThat(resolver.resolve(new LineDisplayInput(
                "raw", null, null, null, null, null, null, null)).name()).isEqualTo("raw");

        // raw 없고 custom 있으면 custom
        assertThat(resolver.resolve(new LineDisplayInput(
                null, "custom", "ingredient", null, null, null, null, null)).name()).isEqualTo("custom");

        // raw, custom 없고 ingredient만 있으면 ingredient
        assertThat(resolver.resolve(new LineDisplayInput(
                null, null, "ingredient", null, null, null, null, null)).name()).isEqualTo("ingredient");
    }

    @Test
    @DisplayName("quantity는 rawQuantityText > legacyQuantity")
    void resolve_quantityFallback() {
        assertThat(resolver.resolve(new LineDisplayInput(
                null, null, null, "3", "999", null, null, null)).quantity()).isEqualTo("3");

        assertThat(resolver.resolve(new LineDisplayInput(
                null, null, null, "  ", "999", null, null, null)).quantity()).isEqualTo("999");
    }

    @Test
    @DisplayName("unit은 rawUnitText > customUnit > legacyUnit")
    void resolve_unitFallback() {
        // raw 우선
        assertThat(resolver.resolve(new LineDisplayInput(
                null, null, null, null, null, "쪽", "큰술", "g")).unit()).isEqualTo("쪽");

        // raw blank → customUnit
        assertThat(resolver.resolve(new LineDisplayInput(
                null, null, null, null, null, "  ", "큰술", "g")).unit()).isEqualTo("큰술");

        // raw, customUnit 모두 blank → legacy
        assertThat(resolver.resolve(new LineDisplayInput(
                null, null, null, null, null, null, "", "g")).unit()).isEqualTo("g");
    }

    @Test
    @DisplayName("모두 null/blank: 결과도 모두 null (호출자가 표시 정책 결정)")
    void resolve_allBlank_returnsNulls() {
        DisplayLine out = resolver.resolve(new LineDisplayInput(
                null, null, null, null, null, null, null, null));

        assertThat(out.name()).isNull();
        assertThat(out.quantity()).isNull();
        assertThat(out.unit()).isNull();
    }

    @Test
    @DisplayName("C' bypass row 케이스: ingredientName 없어도 raw_name으로 표시 가능")
    void resolve_bypassRow_displaysFromRaw() {
        // C' bypass: ingredient_id=null → ingredientName 없음. customName=raw_name=같은 값.
        LineDisplayInput in = new LineDisplayInput(
                "마늘", "마늘", null,
                "1", "1",
                "큰술", "큰술", null
        );

        DisplayLine out = resolver.resolve(in);
        assertThat(out.name()).isEqualTo("마늘");
        assertThat(out.quantity()).isEqualTo("1");
        assertThat(out.unit()).isEqualTo("큰술");
    }
}
