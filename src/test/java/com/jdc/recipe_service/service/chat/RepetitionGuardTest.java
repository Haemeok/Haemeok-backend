package com.jdc.recipe_service.service.chat;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.assertj.core.api.Assertions.assertThat;

class RepetitionGuardTest {

    private final RepetitionGuard guard = new RepetitionGuard();

    @Test
    @DisplayName("null 입력은 null 반환")
    void nullInputReturnsNull() {
        assertThat(guard.guard(null)).isNull();
    }

    @Test
    @DisplayName("빈 문자열은 그대로 반환")
    void emptyStringReturnsAsIs() {
        assertThat(guard.guard("")).isEmpty();
    }

    @Test
    @DisplayName("MIN_CHUNK_LEN×REPEAT_THRESHOLD(30) 미만 길이는 검사 없이 통과")
    void shortTextBelow30CharsPassesThrough() {
        String text = "이 레시피는 매워요.";
        assertThat(guard.guard(text)).isEqualTo(text);
    }

    @Test
    @DisplayName("정상 답변(반복 없음)은 그대로 반환")
    void normalAnswerWithoutRepetitionPassesThrough() {
        String text = "이 레시피는 매콤한 김치찌개에요. 고춧가루 1작은술과 청양고추 1개를 넣어 끓여주시면 됩니다. " +
                "보통 매운맛으로 즐길 수 있고, 매운 정도는 고춧가루 양으로 조절 가능해요.";
        assertThat(guard.guard(text)).isEqualTo(text);
    }

    @Test
    @DisplayName("같은 chunk가 2번만 반복되면 통과 (THRESHOLD=3 미만)")
    void twoTimesRepetitionDoesNotTrigger() {
        String chunk = "고춧가루를 한꼬집 더. ";
        String text = "안녕하세요. " + chunk.repeat(2) + "끝입니다. 마무리합니다.";
        assertThat(guard.guard(text)).isEqualTo(text);
    }

    @Test
    @DisplayName("chunk 12자가 3회 연속 반복되면 첫 발생 위치에서 truncate")
    void chunk12CharsRepeatedThreeTimesGetsTruncated() {
        String chunk = "양파를 다집니다. ";
        String prefix = "안녕하세요. ";
        String text = prefix + chunk.repeat(3) + "끝입니다. 마무리합니다.";

        String result = guard.guard(text);

        assertThat(result).isNotEqualTo(text);
        assertThat(result).endsWith("...");
        assertThat(result.length()).isLessThan(text.length());
        assertThat(result).startsWith("안녕하세요");
    }

    @Test
    @DisplayName("chunk 25자가 3회 반복되면 truncate (MAX 안쪽)")
    void chunk25CharsRepeatedTriggersTruncate() {
        String chunk = "고춧가루 양을 조금 더 줄여서 넣으세요. ";
        String text = "도움말: " + chunk.repeat(3) + "또 있어요.";

        String result = guard.guard(text);

        assertThat(result).isNotEqualTo(text);
        assertThat(result).endsWith("...");
    }

    @Test
    @DisplayName("MIN_CHUNK_LEN=10 미만 패턴 × 부족한 반복은 감지 안 됨")
    void shortPatternWithInsufficientRepetitionNotDetected() {
        // 알려진 한계: 8자 패턴 × 5회 = 40자. chunk_len=16(8의 배수)에서 chunk×3=48자 필요.
        // 40 < 48 → 미감지. 8자 × 6회(48자) 이상이면 감지됨. 메모리 박힘.
        String chunk = "양파 빼세요. ";  // 8 chars
        String text = "안녕! " + chunk.repeat(5) + "끝";  // 4 + 40 + 1 = 45 chars
        assertThat(guard.guard(text)).isEqualTo(text);
    }

    @Test
    @DisplayName("MAX_CHUNK_LEN=30 초과 비주기성 패턴 × 3회는 감지 안 됨")
    void chunkOver30CharsNonPeriodicNotDetected() {
        // 35자 minimum-period 패턴(내부 sub-repeat 없음).
        String chunk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ123456789";  // 35 chars
        assertThat(chunk.length()).isEqualTo(35);

        String text = "intro " + chunk.repeat(3) + " end";  // ~115 chars
        assertThat(guard.guard(text)).isEqualTo(text);
    }

    @Test
    @DisplayName("Solar 무한반복 버그 시뮬레이션 — 짧은 청크 대량 반복")
    void simulateSolarInfiniteRepetitionBug() {
        // Solar가 가끔 같은 문구를 100번 반복하는 버그 → guard가 truncate해야 함
        String chunk = "고춧가루를 빼고 그 대신에 ";  // 15 chars
        String text = "괜찮아요! " + chunk.repeat(50);

        String result = guard.guard(text);

        assertThat(result).endsWith("...");
        assertThat(result.length()).isLessThan(100);  // 50회 반복 대비 훨씬 짧아야
    }

    @Nested
    @DisplayName("자기 참조 (X 대신 X) 검출 (P1, 2026-04-26)")
    class SelfReferencePatternTests {

        @Test
        @DisplayName("실제 발견된 무한반복 답변 (A2 id=33) 검출")
        void detectRealCaseFromMultiTurnA2() {
            String input = """
                    고춧가루를 1작은술 → ½작은술로 줄이면 매운맛 절반 완화.
                    고춧가루 대신 고춧가루는 안 되고… (죄송합니다!)
                    고춧가루 대신 고춧가루는 안 되고…
                    (다시) 고춧가루 대신 고춧가루는 안 되고…
                    """;

            String result = guard.guard(input);

            assertThat(result.length()).isLessThan(input.length());
            assertThat(result).contains("매운맛 절반 완화");
            assertThat(result).doesNotContain("(다시) 고춧가루 대신 고춧가루");
            assertThat(result).endsWith("...");
        }

        @Test
        @DisplayName("X 대신 X 한 번만 발생도 truncate (조심스러운 임계)")
        void selfReferenceSingleOccurrence() {
            String input = "정상 답변. 고춧가루 대신 고춧가루 어쩌고저쩌고.";
            String result = guard.guard(input);
            assertThat(result).doesNotContain("고춧가루 대신 고춧가루");
            assertThat(result).contains("정상 답변.");
        }

        @ParameterizedTest(name = "[{index}] FP guard: \"{0}\"")
        @ValueSource(strings = {
                "마스카포네 치즈 대신 크림치즈를 사용하세요.",
                "고춧가루 대신 파프리카 가루를 추천합니다.",
                "**고춧가루 대신** ½작은술 사용",
                "** 대신 **",
                "양파 대신 대파를 넣어보세요."
        })
        void normalSubstitutionPatternsPass(String input) {
            String result = guard.guard(input);
            assertThat(result)
                    .as("정상 표현인데 자기참조로 오판: %s", input)
                    .isEqualTo(input);
        }

        @Test
        @DisplayName("같은 단어가 가까이 있어도 '대신' 없으면 통과")
        void sameWordWithoutDaesin() {
            String input = "고춧가루를 1작은술 넣고, 고춧가루 양은 조절 가능";
            assertThat(guard.guard(input)).isEqualTo(input);
        }
    }
}
