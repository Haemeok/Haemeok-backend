package com.jdc.recipe_service.service.ingredient.normalize;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class UnitNormalizerTest {

    private final UnitNormalizer normalizer = new UnitNormalizer();

    @Test
    @DisplayName("영문 큰술 alias: T / Tbs / Tbsp / tablespoon → \"큰술\"")
    void normalize_tablespoonAliases() {
        assertThat(normalizer.normalize("T")).isEqualTo("큰술");
        assertThat(normalizer.normalize("Tbs")).isEqualTo("큰술");
        assertThat(normalizer.normalize("Tbsp")).isEqualTo("큰술");
        assertThat(normalizer.normalize("tablespoon")).isEqualTo("큰술");
        assertThat(normalizer.normalize("TABLESPOON")).isEqualTo("큰술");
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: 단일 문자 T/t는 case-sensitive (조리 표기 관습)")
    void normalize_singleCharCase_bigVsSmallSpoon() {
        // 대문자 T는 큰술
        assertThat(normalizer.normalize("T")).isEqualTo("큰술");
        // 소문자 t는 작은술 — lowercase 통일하면 큰술로 잘못 매핑되던 회귀
        assertThat(normalizer.normalize("t")).isEqualTo("작은술");
        // 공백 trim 후에도 case-sensitive 유지
        assertThat(normalizer.normalize(" T ")).isEqualTo("큰술");
        assertThat(normalizer.normalize(" t ")).isEqualTo("작은술");
    }

    @Test
    @DisplayName("영문 작은술 alias: ts / tsp / teaspoon → \"작은술\"")
    void normalize_teaspoonAliases() {
        assertThat(normalizer.normalize("ts")).isEqualTo("작은술");
        assertThat(normalizer.normalize("tsp")).isEqualTo("작은술");
        assertThat(normalizer.normalize("teaspoon")).isEqualTo("작은술");
    }

    @Test
    @DisplayName("한글 단위는 그대로 통과 (canonical로 간주)")
    void normalize_koreanUnitsPassThrough() {
        assertThat(normalizer.normalize("큰술")).isEqualTo("큰술");
        assertThat(normalizer.normalize("작은술")).isEqualTo("작은술");
        assertThat(normalizer.normalize("개")).isEqualTo("개");
        assertThat(normalizer.normalize("쪽")).isEqualTo("쪽");
        assertThat(normalizer.normalize("컵")).isEqualTo("컵");
    }

    @Test
    @DisplayName("앞뒤 공백 제거")
    void normalize_trimsWhitespace() {
        assertThat(normalizer.normalize(" 큰술 ")).isEqualTo("큰술");
        assertThat(normalizer.normalize("\tg\n")).isEqualTo("g");
    }

    @Test
    @DisplayName("대소문자 정규화: G → g, ML → ml")
    void normalize_caseInsensitive() {
        assertThat(normalizer.normalize("G")).isEqualTo("g");
        assertThat(normalizer.normalize("ML")).isEqualTo("ml");
        assertThat(normalizer.normalize("Kg")).isEqualTo("kg");
    }

    @Test
    @DisplayName("cc → ml로 매핑")
    void normalize_ccToMl() {
        assertThat(normalizer.normalize("cc")).isEqualTo("ml");
        assertThat(normalizer.normalize("CC")).isEqualTo("ml");
    }

    @Test
    @DisplayName("blank/null 입력: 빈 문자열")
    void normalize_blankInputs() {
        assertThat(normalizer.normalize(null)).isEmpty();
        assertThat(normalizer.normalize("")).isEmpty();
        assertThat(normalizer.normalize("   ")).isEmpty();
    }

    @Test
    @DisplayName("**SHOULD 회귀 차단**: 모르는 한글 단위는 raw 보존 (정책: raw-preserving)")
    void normalize_unknownKoreanUnit_preserved() {
        assertThat(normalizer.normalize("국자")).isEqualTo("국자");
        assertThat(normalizer.normalize("주걱")).isEqualTo("주걱");
        assertThat(normalizer.normalize("봉지")).isEqualTo("봉지");
    }

    @Test
    @DisplayName("모르는 영문 단위는 lowercase로만 정규화")
    void normalize_unknownEnglishUnit_lowercased() {
        assertThat(normalizer.normalize("Dash")).isEqualTo("dash");
        assertThat(normalizer.normalize("PINCH")).isEqualTo("pinch");
    }
}
