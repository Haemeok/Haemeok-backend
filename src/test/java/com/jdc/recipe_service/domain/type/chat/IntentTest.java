package com.jdc.recipe_service.domain.type.chat;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;

import static org.assertj.core.api.Assertions.assertThat;

class IntentTest {

    @ParameterizedTest
    @CsvSource({
            "IN_SCOPE, IN_SCOPE",
            "OUT_OF_SCOPE, OUT_OF_SCOPE",
            "UNCLEAR, UNCLEAR"
    })
    @DisplayName("정확 일치 라벨은 그대로 매칭")
    void exactLabelMatching(String input, Intent expected) {
        assertThat(Intent.fromString(input)).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource({
            "in_scope, IN_SCOPE",
            "out_of_scope, OUT_OF_SCOPE",
            "Unclear, UNCLEAR"
    })
    @DisplayName("소문자/혼합 대소문자도 toUpperCase로 정규화 매칭")
    void caseInsensitiveMatching(String input, Intent expected) {
        assertThat(Intent.fromString(input)).isEqualTo(expected);
    }

    @Test
    @DisplayName("앞뒤 공백 trim 후 매칭 (IN_SCOPE)")
    void trimsLeadingTrailingSpacesInScope() {
        assertThat(Intent.fromString("  IN_SCOPE  ")).isEqualTo(Intent.IN_SCOPE);
    }

    @Test
    @DisplayName("탭/개행 포함 trim 매칭 (OUT_OF_SCOPE)")
    void trimsTabAndNewlineOutOfScope() {
        assertThat(Intent.fromString("\tOUT_OF_SCOPE\n")).isEqualTo(Intent.OUT_OF_SCOPE);
    }

    @Test
    @DisplayName("앞뒤 공백 trim (UNCLEAR)")
    void trimsLeadingTrailingSpacesUnclear() {
        assertThat(Intent.fromString("  UNCLEAR  ")).isEqualTo(Intent.UNCLEAR);
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "IN_SCOPE.",
            "IN_SCOPE\n",
            "IN_SCOPE 라고 분류했어요",
            "분류: IN_SCOPE"
    })
    @DisplayName("contains 매칭 — 라벨이 부분 포함되어도 매칭")
    void containsMatching(String input) {
        assertThat(Intent.fromString(input)).isEqualTo(Intent.IN_SCOPE);
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"   ", "INVALID_LABEL", "RANDOM_TEXT", "잘 모르겠음"})
    @DisplayName("매칭 실패 시 UNKNOWN fallback")
    void unknownFallback(String input) {
        assertThat(Intent.fromString(input)).isEqualTo(Intent.UNKNOWN);
    }

    @Test
    @DisplayName("우선순위 — 두 라벨 동시 포함 시 enum 선언 순서대로 (IN_SCOPE 먼저)")
    void declarationOrderPriority() {
        // values() = [IN_SCOPE, OUT_OF_SCOPE, UNCLEAR, UNKNOWN]
        // 입력 "OUT_OF_SCOPE IN_SCOPE"에서 contains("IN_SCOPE")가 먼저 검사됨 → IN_SCOPE 매칭
        assertThat(Intent.fromString("OUT_OF_SCOPE IN_SCOPE")).isEqualTo(Intent.IN_SCOPE);
    }

    @Test
    @DisplayName("OUT_OF_SCOPE 단독은 IN_SCOPE 매칭 안 됨 (substring 안 겹침)")
    void outOfScopeDoesNotMatchInScope() {
        // "OUT_OF_SCOPE"에 "IN_SCOPE" 부분 문자열 없음을 보장
        assertThat(Intent.fromString("OUT_OF_SCOPE")).isEqualTo(Intent.OUT_OF_SCOPE);
    }
}
