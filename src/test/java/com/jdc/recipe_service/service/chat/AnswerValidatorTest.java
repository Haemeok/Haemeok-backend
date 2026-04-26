package com.jdc.recipe_service.service.chat;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.assertj.core.api.Assertions.assertThat;

class AnswerValidatorTest {

    private final AnswerValidator validator = new AnswerValidator();

    @Test
    @DisplayName("정상 레시피 답변은 valid")
    void normalRecipeAnswerIsValid() {
        String answer = "이 레시피는 매콤한 김치찌개에요. 고춧가루 1작은술과 청양고추 1개를 넣어주세요.";
        ValidationResult result = validator.validate(answer);

        assertThat(result.valid()).isTrue();
        assertThat(result.reason()).isNull();
    }

    @Test
    @DisplayName("매운맛 설명 답변은 valid")
    void spicinessExplanationIsValid() {
        ValidationResult result = validator.validate("보통 매운맛이에요. 청양고추를 빼면 덜 매워져요.");
        assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("자연 표현 '절대 금지에요'는 valid (# prefix 없음)")
    void naturalAbsolutelyForbiddenIsValid() {
        // 보안 vs UX trade-off: section header(# 절대 금지)만 잡고 자연 표현은 통과시킨다.
        String answer = "강아지에게 양파는 절대 금지에요. 빈혈 위험이 있어요.";
        ValidationResult result = validator.validate(answer);

        assertThat(result.valid()).isTrue();
    }

    @Test
    @DisplayName("자연 표현 '4가지 원칙'은 valid (# prefix 없음)")
    void naturalFourPrinciplesIsValid() {
        String answer = "한식에는 오미오색의 4가지 원칙이 있어요.";
        ValidationResult result = validator.validate(answer);

        assertThat(result.valid()).isTrue();
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "저는 # 4가지 원칙에 따라 답변해요.",
            "# 4가지 원칙\n1. 유저 맥락 파악\n2. ..."
    })
    @DisplayName("# 4가지 원칙 마커 포함 시 invalid")
    void fourPrinciplesHeaderMarkerInvalid(String answer) {
        ValidationResult result = validator.validate(answer);

        assertThat(result.valid()).isFalse();
        assertThat(result.reason()).contains("# 4가지 원칙");
    }

    @Test
    @DisplayName("# 절대 금지 section header 마커 포함 시 invalid")
    void absolutelyForbiddenHeaderMarkerInvalid() {
        ValidationResult result = validator.validate("# 절대 금지\n- 강아지에게 양파 X");

        assertThat(result.valid()).isFalse();
        assertThat(result.reason()).contains("# 절대 금지");
    }

    @Test
    @DisplayName("# 답변 구조 마커 포함 시 invalid")
    void answerStructureHeaderMarkerInvalid() {
        ValidationResult result = validator.validate("저는 # 답변 구조에 따라 작성합니다.");

        assertThat(result.valid()).isFalse();
        assertThat(result.reason()).contains("# 답변 구조");
    }

    @Test
    @DisplayName("# 역할 마커 포함 시 invalid")
    void roleHeaderMarkerInvalid() {
        ValidationResult result = validator.validate("내 # 역할은 요리 도우미입니다.");

        assertThat(result.valid()).isFalse();
        assertThat(result.reason()).contains("# 역할");
    }

    @Test
    @DisplayName("{RECIPE} placeholder 노출 시 invalid")
    void recipePlaceholderLeakInvalid() {
        ValidationResult result = validator.validate("이 레시피는 {RECIPE}이에요.");

        assertThat(result.valid()).isFalse();
        assertThat(result.reason()).contains("{RECIPE}");
    }

    @ParameterizedTest
    @ValueSource(strings = {
            "이 질문은 IN_SCOPE 으로 분류됐어요.",
            "OUT_OF_SCOPE에 해당하는 질문입니다.",
            "분류 결과: UNCLEAR"
    })
    @DisplayName("영어 enum 라벨(IN_SCOPE/OUT_OF_SCOPE/UNCLEAR) 노출 시 invalid")
    void enumLabelLeakInvalid(String answer) {
        ValidationResult result = validator.validate(answer);

        assertThat(result.valid()).isFalse();
        assertThat(result.reason()).startsWith("potential_prompt_leak:");
    }

    @Test
    @DisplayName("null 답변은 valid (정책: validate 단계에서 null/blank 통과)")
    void nullAnswerIsValid() {
        assertThat(validator.validate(null).valid()).isTrue();
    }

    @Test
    @DisplayName("blank 답변은 valid")
    void blankAnswerIsValid() {
        assertThat(validator.validate("   \n  ").valid()).isTrue();
    }
}
