package com.jdc.recipe_service.dev.service.youtube;

import com.jdc.recipe_service.dev.service.youtube.YoutubeSignalDetector.SignalReport;
import com.jdc.recipe_service.domain.type.media.EvidenceLevel;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * YoutubeSignalDetector 단위 테스트.
 *
 * 검증 포인트:
 *  1. detectSignals: 자막/설명/댓글 신호를 독립적으로 boolean으로 환산
 *  2. computeEvidence: usedGemini 우선 (LOW), 그 외 hasSubtitle 우선 (HIGH), 마지막 MEDIUM
 *  3. boundary: 자막 50자 미만은 hasSubtitle=false (V2 기준 일치)
 */
class YoutubeSignalDetectorTest {

    private final YoutubeSignalDetector detector = new YoutubeSignalDetector();

    // --- detectSignals ---

    @Test
    @DisplayName("자막 길이 50자 초과: hasSubtitle=true")
    void detectSignals_subtitleAboveThreshold() {
        String script = "a".repeat(60);
        SignalReport r = detector.detectSignals("", "", script);
        assertThat(r.hasSubtitle()).isTrue();
    }

    @Test
    @DisplayName("자막 길이 50자 정확히 일치: hasSubtitle=false (V2 기준 length > 50)")
    void detectSignals_subtitleAtThreshold() {
        String script = "a".repeat(50);
        SignalReport r = detector.detectSignals("", "", script);
        assertThat(r.hasSubtitle()).isFalse();
    }

    @Test
    @DisplayName("자막 null: hasSubtitle=false")
    void detectSignals_subtitleNull() {
        SignalReport r = detector.detectSignals("desc", "cmt", null);
        assertThat(r.hasSubtitle()).isFalse();
    }

    @Test
    @DisplayName("설명에 재료 키워드 (재료, 양념 등) 포함: hasDescriptionIngredient=true")
    void detectSignals_descriptionWithIngredientKeyword() {
        SignalReport r = detector.detectSignals(
                "오늘은 김치찌개를 만들어볼게요. 재료: 김치 200g, 돼지고기 100g",
                "", "");
        assertThat(r.hasDescriptionIngredient()).isTrue();
    }

    @Test
    @DisplayName("설명에 영어 'ingredient' 키워드 포함: hasDescriptionIngredient=true (case-insensitive)")
    void detectSignals_descriptionWithEnglishKeyword() {
        SignalReport r = detector.detectSignals(
                "Today's RECIPE - Ingredients listed below",
                "", "");
        assertThat(r.hasDescriptionIngredient()).isTrue();
    }

    @Test
    @DisplayName("설명에 키워드 없음: hasDescriptionIngredient=false")
    void detectSignals_descriptionWithoutKeyword() {
        SignalReport r = detector.detectSignals(
                "맛있게 드세요. 좋아요와 구독 부탁드립니다",
                "", "");
        assertThat(r.hasDescriptionIngredient()).isFalse();
    }

    @Test
    @DisplayName("댓글에 키워드 포함: hasCommentIngredient=true")
    void detectSignals_commentWithIngredientKeyword() {
        SignalReport r = detector.detectSignals(
                "", "좋은 영상이네요! 양념 비율이 어떻게 되나요?", "");
        assertThat(r.hasCommentIngredient()).isTrue();
    }

    @Test
    @DisplayName("3개 신호 모두 독립적으로 검출됨")
    void detectSignals_independent() {
        SignalReport r = detector.detectSignals(
                "재료 목록",                  // desc → true
                "맛있어 보여요",              // cmt → false
                "a".repeat(60)               // script → true
        );
        assertThat(r.hasDescriptionIngredient()).isTrue();
        assertThat(r.hasCommentIngredient()).isFalse();
        assertThat(r.hasSubtitle()).isTrue();
    }

    // --- computeEvidence ---

    @Test
    @DisplayName("usedGeminiAnalysis=true이면 다른 신호 무관하게 LOW")
    void computeEvidence_geminiUsed_isLow() {
        SignalReport perfectSignals = new SignalReport(true, true, true);
        EvidenceLevel level = detector.computeEvidence(perfectSignals, true);
        assertThat(level).isEqualTo(EvidenceLevel.LOW);
    }

    @Test
    @DisplayName("Gemini 미사용 + 자막 있음 → HIGH (가장 강한 근거)")
    void computeEvidence_noGeminiWithSubtitle_isHigh() {
        SignalReport signals = new SignalReport(true, false, false);
        EvidenceLevel level = detector.computeEvidence(signals, false);
        assertThat(level).isEqualTo(EvidenceLevel.HIGH);
    }

    @Test
    @DisplayName("Gemini 미사용 + 자막 없음 → MEDIUM (설명/댓글 신호로 추출됨)")
    void computeEvidence_noGeminiNoSubtitle_isMedium() {
        SignalReport signals = new SignalReport(false, true, true);
        EvidenceLevel level = detector.computeEvidence(signals, false);
        assertThat(level).isEqualTo(EvidenceLevel.MEDIUM);
    }

    @Test
    @DisplayName("Gemini 미사용 + 자막 있음 + 설명 신호 같이 있어도 HIGH (자막이 우선)")
    void computeEvidence_subtitlePrecedence() {
        SignalReport signals = new SignalReport(true, true, true);
        EvidenceLevel level = detector.computeEvidence(signals, false);
        assertThat(level).isEqualTo(EvidenceLevel.HIGH);
    }
}
