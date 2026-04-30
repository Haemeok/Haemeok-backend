package com.jdc.recipe_service.service.ingredient.normalize;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class QuantityParserTest {

    private final QuantityParser parser = new QuantityParser();

    @Test
    @DisplayName("정수 \"3\" 파싱: amount=3, special=false")
    void parse_integer() {
        ParsedQuantity result = parser.parse("3");
        assertThat(result.amount()).isEqualByComparingTo("3");
        assertThat(result.special()).isFalse();
        assertThat(result.hasAmount()).isTrue();
        assertThat(result.raw()).isEqualTo("3");
    }

    @Test
    @DisplayName("소수 \"0.5\" 파싱: amount=0.5")
    void parse_decimal() {
        ParsedQuantity result = parser.parse("0.5");
        assertThat(result.amount()).isEqualByComparingTo("0.5");
        assertThat(result.special()).isFalse();
    }

    @Test
    @DisplayName("단순분수 \"1/2\" 파싱: amount=0.5")
    void parse_simpleFraction() {
        ParsedQuantity result = parser.parse("1/2");
        assertThat(result.amount()).isEqualByComparingTo("0.5");
        assertThat(result.special()).isFalse();
    }

    @Test
    @DisplayName("0이 아닌 분수 \"1/3\" 파싱: 6자리 정밀도 보존")
    void parse_oneThird_keepsPrecision() {
        ParsedQuantity result = parser.parse("1/3");
        assertThat(result.amount()).isEqualByComparingTo("0.333333");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: \"약간\"은 amount=null + special=true (입력 누락과 구분)")
    void parse_yakgan_isSpecialNotBlank() {
        ParsedQuantity result = parser.parse("약간");
        assertThat(result.amount()).isNull();
        assertThat(result.special()).isTrue();
        assertThat(result.hasAmount()).isFalse();
        assertThat(result.raw()).isEqualTo("약간");
    }

    @Test
    @DisplayName("\"적당량\"도 special=true")
    void parse_jeokdangryang_isSpecial() {
        ParsedQuantity result = parser.parse("적당량");
        assertThat(result.special()).isTrue();
    }

    @Test
    @DisplayName("blank 입력: amount=null + special=false")
    void parse_blank_returnsBlank() {
        assertThat(parser.parse("").special()).isFalse();
        assertThat(parser.parse("   ").special()).isFalse();
        assertThat(parser.parse("").amount()).isNull();
    }

    @Test
    @DisplayName("null 입력: blank 동등 처리, raw=null 보존")
    void parse_null_returnsBlankWithNullRaw() {
        ParsedQuantity result = parser.parse(null);
        assertThat(result.amount()).isNull();
        assertThat(result.special()).isFalse();
        assertThat(result.raw()).isNull();
    }

    @Test
    @DisplayName("앞뒤 공백 제거 후 파싱")
    void parse_trimsWhitespace() {
        assertThat(parser.parse(" 3 ").amount()).isEqualByComparingTo("3");
        assertThat(parser.parse("\t1/2\n").amount()).isEqualByComparingTo("0.5");
    }

    @Test
    @DisplayName("0 분모 분수: 파싱 실패 (special=false, amount=null)")
    void parse_divisionByZero_returnsBlank() {
        ParsedQuantity result = parser.parse("1/0");
        assertThat(result.amount()).isNull();
        assertThat(result.special()).isFalse();
    }

    @Test
    @DisplayName("이상한 문자열: blank 처리 (예외 던지지 않음)")
    void parse_garbage_returnsBlankNotException() {
        ParsedQuantity result = parser.parse("abc!@#");
        assertThat(result.amount()).isNull();
        assertThat(result.special()).isFalse();
        assertThat(result.raw()).isEqualTo("abc!@#");
    }

    @Test
    @DisplayName("BigDecimal 직접 비교: \"2\"와 \"2.0\"은 같은 amount")
    void parse_integerAndDecimalEquivalent() {
        BigDecimal a = parser.parse("2").amount();
        BigDecimal b = parser.parse("2.0").amount();
        assertThat(a).isEqualByComparingTo(b);
    }
}
