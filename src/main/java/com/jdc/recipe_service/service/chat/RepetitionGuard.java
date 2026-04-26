package com.jdc.recipe_service.service.chat;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@Component
public class RepetitionGuard {

    private static final int REPEAT_THRESHOLD = 3;
    private static final int MIN_CHUNK_LEN = 10;
    private static final int MAX_CHUNK_LEN = 30;

    // 자기 참조 검출 (P1, 2026-04-26): A2 multi-turn에서 발견된 "X 대신 X" stuck loop.
    // 200건 csv FP 0 검증. markdown ** 배제 위해 [가-힣A-Za-z] word-class 제한 필수.
    private static final Pattern SELF_REFERENCE =
            Pattern.compile("([가-힣A-Za-z]{2,10})\\s*대신\\s*\\1");

    public String guard(String text) {
        if (text == null || text.isBlank()) return text;

        Matcher selfRef = SELF_REFERENCE.matcher(text);
        if (selfRef.find()) {
            int safeEnd = findSafeBoundary(text, selfRef.start());
            log.warn("Self-reference loop detected: matched=\"{}\" truncatedAt={}",
                    selfRef.group(0), safeEnd);
            return text.substring(0, safeEnd).trim() + "...";
        }

        if (text.length() < MIN_CHUNK_LEN * REPEAT_THRESHOLD) {
            return text;
        }
        for (int len = MIN_CHUNK_LEN; len <= MAX_CHUNK_LEN; len++) {
            Integer cutoff = findRepetitionCutoff(text, len);
            if (cutoff != null) {
                return text.substring(0, cutoff).trim() + "...";
            }
        }
        return text;
    }

    private int findSafeBoundary(String text, int idx) {
        for (int i = idx - 1; i > 0; i--) {
            char c = text.charAt(i);
            if (c == '.' || c == '!' || c == '?' || c == '\n') {
                return i + 1;
            }
        }
        for (int i = idx - 1; i > 0; i--) {
            if (Character.isWhitespace(text.charAt(i))) {
                return i;
            }
        }
        return Math.max(idx, 100);
    }

    private Integer findRepetitionCutoff(String text, int chunkLen) {
        int maxStart = text.length() - chunkLen * REPEAT_THRESHOLD;
        for (int i = 0; i <= maxStart; i++) {
            String chunk = text.substring(i, i + chunkLen);
            if (chunk.trim().isEmpty()) continue;

            String repeated = chunk.repeat(REPEAT_THRESHOLD);
            int idx = text.indexOf(repeated);
            if (idx >= 0) {
                return idx;
            }
        }
        return null;
    }
}
