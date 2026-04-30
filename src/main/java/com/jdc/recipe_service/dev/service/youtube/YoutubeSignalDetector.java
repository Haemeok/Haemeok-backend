package com.jdc.recipe_service.dev.service.youtube;

import com.jdc.recipe_service.dev.util.youtube.YoutubeExtractionHelpers;
import com.jdc.recipe_service.domain.type.media.EvidenceLevel;
import org.springframework.stereotype.Component;

/**
 * YouTube 추출 신호 감지 + evidence_level 판정.
 *
 * RecipeYoutubeExtractionInfo에 저장될 4개 boolean과 evidence_level을 일관된 규칙으로 산출한다.
 *  - hasSubtitle              : 자막(script)이 의미 있게 존재 (V2의 hasSubtitleData와 동일 기준: length > 50)
 *  - hasDescriptionIngredient : 설명에 재료 키워드 (INGREDIENT_KEYWORD_PATTERN) 매치
 *  - hasCommentIngredient     : 댓글에 재료 키워드 매치
 *  - usedGeminiAnalysis       : 추출 흐름에서 Gemini Multimodal fallback이 실제로 사용되었는지 (외부 입력)
 *
 * EvidenceLevel 판정 (운영 V2의 isTextSufficient 분기와 정렬):
 *  - LOW    : Gemini fallback 사용 (텍스트 정보 부족)
 *  - HIGH   : Gemini 미사용 + 자막 있음 (가장 정확한 근거)
 *  - MEDIUM : Gemini 미사용 + 자막 없지만 설명/댓글 신호로 추출 성공
 */
@Component
public class YoutubeSignalDetector {

    /** 자막의 최소 의미 길이. V2 isTextSufficient의 hasSubtitleData 기준과 동일. */
    static final int MIN_SUBTITLE_LENGTH = 50;

    /**
     * 추출 결과 metadata 신호 검사.
     * (조회는 즉시 수행 — yt-dlp 응답 직후 호출하면 됨.)
     */
    public SignalReport detectSignals(String description, String comments, String scriptPlain) {
        boolean hasSubtitle = scriptPlain != null && scriptPlain.length() > MIN_SUBTITLE_LENGTH;

        boolean hasDescriptionIngredient = matchesIngredientKeyword(description);
        boolean hasCommentIngredient = matchesIngredientKeyword(comments);

        return new SignalReport(hasSubtitle, hasDescriptionIngredient, hasCommentIngredient);
    }

    /**
     * 추출 흐름 결정 직후 evidence_level 산출.
     *
     * @param signals          추출 시작 시점에 캡처한 신호
     * @param usedGeminiAnalysis 실제 Gemini fallback 사용 여부 (false면 Grok-only 경로로 성공)
     */
    public EvidenceLevel computeEvidence(SignalReport signals, boolean usedGeminiAnalysis) {
        if (usedGeminiAnalysis) {
            return EvidenceLevel.LOW;
        }
        if (signals.hasSubtitle()) {
            return EvidenceLevel.HIGH;
        }
        // Grok 성공 + 자막 없음 → 설명/댓글 신호로 추출됨
        return EvidenceLevel.MEDIUM;
    }

    private boolean matchesIngredientKeyword(String text) {
        if (text == null || text.isBlank()) return false;
        return YoutubeExtractionHelpers.INGREDIENT_KEYWORD_PATTERN.matcher(text).find();
    }

    /**
     * 추출 신호 캡처 결과.
     * RecipeYoutubeExtractionInfo entity에 저장되는 3개 boolean과 1:1 대응.
     */
    public record SignalReport(
            boolean hasSubtitle,
            boolean hasDescriptionIngredient,
            boolean hasCommentIngredient
    ) {}
}
