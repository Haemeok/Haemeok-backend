package com.jdc.recipe_service.service.chat;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class InputSanitizerTest {

    @Test
    @DisplayName("null 입력은 null 반환")
    void nullInputReturnsNull() {
        assertThat(InputSanitizer.sanitize(null)).isNull();
    }

    @Test
    @DisplayName("빈 문자열은 빈 문자열 반환")
    void emptyStringReturnsEmpty() {
        assertThat(InputSanitizer.sanitize("")).isEmpty();
    }

    @Test
    @DisplayName("공백만 있는 입력은 trim되어 빈 문자열")
    void whitespaceOnlyReturnsEmpty() {
        assertThat(InputSanitizer.sanitize("    \t\n  ")).isEmpty();
    }

    @Test
    @DisplayName("정상 한국어 질문은 변형 없이 통과")
    void normalKoreanQuestionPassesThrough() {
        String input = "이거 매워요?";
        assertThat(InputSanitizer.sanitize(input)).isEqualTo(input);
    }

    @Test
    @DisplayName("script 태그는 제거하되 content는 텍스트로 보존 (저장형 XSS 방어 충분)")
    void removesScriptTagButKeepsContent() {
        // 정책: 태그만 제거. content는 plain text로 남김 → DB/로그/admin UI에서 텍스트로만 표시.
        // 실행 컨텍스트(브라우저 innerHTML 등)에서의 XSS는 출력 시 추가 escape 책임.
        String raw = "<script>alert('xss')</script>이거 매워요?";
        assertThat(InputSanitizer.sanitize(raw)).isEqualTo("alert('xss')이거 매워요?");
    }

    @Test
    @DisplayName("p 태그 제거")
    void removesParagraphTag() {
        assertThat(InputSanitizer.sanitize("<p>일반 텍스트</p>")).isEqualTo("일반 텍스트");
    }

    @Test
    @DisplayName("a href 태그 제거")
    void removesAnchorTagWithAttrs() {
        String raw = "<a href=\"evil.com\">클릭</a> 양파 빼주세요";
        assertThat(InputSanitizer.sanitize(raw)).isEqualTo("클릭 양파 빼주세요");
    }

    @Test
    @DisplayName("img onerror 태그 제거")
    void removesImgWithEventHandler() {
        String raw = "<img src=x onerror=alert(1)>안녕";
        assertThat(InputSanitizer.sanitize(raw)).isEqualTo("안녕");
    }

    @Test
    @DisplayName("연속 공백 정규화")
    void collapsesMultipleSpaces() {
        assertThat(InputSanitizer.sanitize("양파   넣어요")).isEqualTo("양파 넣어요");
    }

    @Test
    @DisplayName("앞뒤 공백 trim")
    void trimsLeadingTrailingSpaces() {
        assertThat(InputSanitizer.sanitize("  앞뒤 공백 정리  ")).isEqualTo("앞뒤 공백 정리");
    }

    @Test
    @DisplayName("탭은 공백으로 변환·정규화")
    void tabsBecomeSingleSpace() {
        assertThat(InputSanitizer.sanitize("탭\t\t사이도 공백")).isEqualTo("탭 사이도 공백");
    }

    @Test
    @DisplayName("개행은 공백으로 변환·정규화")
    void newlinesBecomeSingleSpace() {
        assertThat(InputSanitizer.sanitize("개행\n\n공백")).isEqualTo("개행 공백");
    }

    @Test
    @DisplayName("탭·개행·공백 혼합 정규화")
    void mixedWhitespaceCollapsed() {
        assertThat(InputSanitizer.sanitize("혼합\t \n  공백 여러개")).isEqualTo("혼합 공백 여러개");
    }

    @Test
    @DisplayName("HTML + 공백 복합 정리")
    void htmlAndWhitespaceCombo() {
        String raw = "  <div>  양파를 \n\n 넣어요   </div>  ";
        assertThat(InputSanitizer.sanitize(raw)).isEqualTo("양파를 넣어요");
    }
}
