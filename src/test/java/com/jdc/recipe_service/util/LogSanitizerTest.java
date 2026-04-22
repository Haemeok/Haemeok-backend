package com.jdc.recipe_service.util;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class LogSanitizerTest {

    @Test
    @DisplayName("URL의 ?key=... 쿼리 파라미터를 마스킹한다")
    void masksUrlApiKeyQueryParam() {
        String input = "I/O error on POST request for \"https://aiplatform.googleapis.com/v1/projects/p/locations/us-central1/publishers/google/models/gemini:generateContent?key=AIzaSyD-abc-real-secret\": connect timed out";

        String masked = LogSanitizer.mask(input);

        assertThat(masked).doesNotContain("AIzaSyD-abc-real-secret");
        assertThat(masked).contains("key=***");
        assertThat(masked).contains("connect timed out");
        assertThat(masked).contains("https://aiplatform.googleapis.com");
    }

    @Test
    @DisplayName("&key= 처럼 중간 위치 쿼리도 마스킹한다")
    void masksKeyInMiddleOfQueryString() {
        String input = "https://example.com/api?foo=bar&key=secret-value-123&x=y";

        String masked = LogSanitizer.mask(input);

        assertThat(masked).doesNotContain("secret-value-123");
        assertThat(masked).contains("&key=***");
        assertThat(masked).contains("foo=bar");
        assertThat(masked).contains("x=y");
    }

    @Test
    @DisplayName("Authorization Bearer 토큰을 마스킹한다")
    void masksBearerToken() {
        String input = "failed with header Authorization: Bearer eyJhbGciOiJIUzI1NiJ9.token.signature";

        String masked = LogSanitizer.mask(input);

        assertThat(masked).doesNotContain("eyJhbGciOiJIUzI1NiJ9.token.signature");
        assertThat(masked).contains("Bearer ***");
    }

    @Test
    @DisplayName("x-goog-api-key 헤더 값을 마스킹한다")
    void masksGoogApiKeyHeader() {
        String input = "request headers: x-goog-api-key: AIzaSyXYZ-test-key, content-type: application/json";

        String masked = LogSanitizer.mask(input);

        assertThat(masked).doesNotContain("AIzaSyXYZ-test-key");
        assertThat(masked).contains("x-goog-api-key: ***");
        assertThat(masked).contains("content-type: application/json");
    }

    @Test
    @DisplayName("비밀이 포함되지 않은 메시지는 그대로 둔다")
    void leavesMessageWithoutSecretsUnchanged() {
        String input = "connect timed out";

        String masked = LogSanitizer.mask(input);

        assertThat(masked).isEqualTo("connect timed out");
    }

    @Test
    @DisplayName("null 입력 시 'null' 문자열을 반환한다")
    void nullInputReturnsNullString() {
        assertThat(LogSanitizer.mask(null)).isEqualTo("null");
    }

    @Test
    @DisplayName("한 메시지 안에 여러 비밀이 있으면 전부 마스킹한다")
    void masksMultipleSecretsInSameMessage() {
        String input = "Retry failed: original URL https://api.example.com/v1?key=first-secret then fallback with Authorization: Bearer fallback-token-xyz";

        String masked = LogSanitizer.mask(input);

        assertThat(masked).doesNotContain("first-secret");
        assertThat(masked).doesNotContain("fallback-token-xyz");
        assertThat(masked).contains("key=***");
        assertThat(masked).contains("Bearer ***");
    }
}
