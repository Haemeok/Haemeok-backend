package com.jdc.recipe_service.service.ai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import io.github.resilience4j.timelimiter.annotation.TimeLimiter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
public class GeminiMultimodalService {

    private final WebClient webClient;
    private final ObjectMapper objectMapper;

    @Value("${gemini.studio.api-key}")
    private String geminiApiKey;

    private static final String MODEL_ID = "gemini-3-flash-preview";

    private static final String BASE_URL = "https://generativelanguage.googleapis.com";
    private static final Duration GENERATE_TIMEOUT = Duration.ofSeconds(600);

    public GeminiMultimodalService(ObjectMapper objectMapper) {
        this.webClient = WebClient.builder()
                .codecs(c -> c.defaultCodecs().maxInMemorySize(16 * 1024 * 1024))
                .baseUrl(BASE_URL)
                .build();
        this.objectMapper = objectMapper;
    }

    /* =========================================================
     * 1. 텍스트 기반 분석
     * ========================================================= */
    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerateText")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerateText")
    public CompletableFuture<RecipeCreateRequestDto> generateRecipeFromText(String systemContent, String fullTextContext) {
        log.info("📝 Gemini(Text) 요청 시작. 텍스트 길이: {}", fullTextContext.length());

        Map<String, Object> requestBody = Map.of(
                "systemInstruction", Map.of("parts", List.of(Map.of("text", systemContent))),
                "contents", List.of(Map.of(
                        "role", "user",
                        "parts", List.of(Map.of(
                                "text", "다음은 유튜브 영상의 텍스트 데이터(자막, 설명, 댓글)입니다. 이를 분석해 레시피를 추출하세요.\n" +
                                        "만약 정보가 부족하거나 레시피가 아니라면, JSON의 'isRecipe' 필드를 false로 반환하세요.\n" +
                                        "억지로 지어내지 마세요.\n\n" +
                                        fullTextContext
                        ))
                )),
                "generationConfig", Map.of(
                        "responseMimeType", "application/json",
                        "temperature", 0.4
                )
        );

        return callGeminiApi(requestBody);
    }

    /* =========================================================
     * 2. 유튜브 URL 기반 분석
     * ========================================================= */
    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerateUrl")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerateUrl")
    @TimeLimiter(name = "aiGenerate", fallbackMethod = "fallbackGenerateUrl")
    public CompletableFuture<RecipeCreateRequestDto> generateRecipeFromYoutubeUrl(String systemContent, String userPrompt, String youtubeUrl) {
        log.info("🎥 Gemini(Youtube URL) 요청: {}", youtubeUrl);

        Map<String, Object> requestBody = Map.of(
                "systemInstruction", Map.of("parts", List.of(Map.of("text", systemContent))),
                "contents", List.of(Map.of(
                        "role", "user",
                        "parts", List.of(
                                Map.of("file_data", Map.of(
                                        "mime_type", "video/mp4",
                                        "file_uri", youtubeUrl
                                )),
                                Map.of("text", "영상 제목: " + userPrompt + "\n\n" +
                                        "위 영상을 처음부터 끝까지 보고 레시피를 추출해줘.\n" +
                                        "영상 화면의 행동과 자막, 소리를 모두 참고해서 정확하게 작성해.")
                        )
                )),
                "generationConfig", Map.of(
                        "responseMimeType", "application/json",
                        "temperature", 0.4
                )
        );

        return callGeminiApi(requestBody);
    }

    /* =========================================================
     * Internal: API Call & Parsing
     * ========================================================= */

    private CompletableFuture<RecipeCreateRequestDto> callGeminiApi(Map<String, Object> requestBody) {
        return webClient.post()
                .uri("/v1beta/models/" + MODEL_ID + ":generateContent")
                .header("x-goog-api-key", geminiApiKey)
                .contentType(MediaType.APPLICATION_JSON)
                .bodyValue(requestBody)
                .retrieve()
                .onStatus(status -> status.isError(), response ->
                        response.bodyToMono(String.class).flatMap(errorBody -> {
                            log.error("Gemini API Error: {}", errorBody);
                            return Mono.error(new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Gemini 통신 실패"));
                        })
                )
                .bodyToMono(JsonNode.class)
                .timeout(GENERATE_TIMEOUT)
                .map(root -> {
                    JsonNode candidates = root.path("candidates");
                    if (candidates.isMissingNode() || candidates.isEmpty()) {
                        log.warn("Gemini returned no candidates. Prompt feedback: {}", root.path("promptFeedback"));
                        throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 응답 후보 없음");
                    }
                    return candidates.get(0).path("content").path("parts").get(0).path("text").asText();
                })
                .flatMap(this::parseResponse)
                .toFuture();
    }

    private Mono<RecipeCreateRequestDto> parseResponse(String rawResponse) {
        return Mono.fromCallable(() -> {
            if (rawResponse == null || rawResponse.isBlank()) throw new RuntimeException("Empty response");

            String cleanedJson = rawResponse.replaceAll("```json", "").replaceAll("```", "").trim();

            int start = cleanedJson.indexOf("{");
            int end = cleanedJson.lastIndexOf("}");
            if (start == -1 || end == -1) throw new RuntimeException("Invalid JSON format");

            cleanedJson = cleanedJson.substring(start, end + 1);
            return objectMapper.readValue(cleanedJson, RecipeCreateRequestDto.class);
        }).onErrorResume(e -> {
            log.error("JSON Parsing Failed. Raw: {}", rawResponse);
            return Mono.error(new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "AI 응답 파싱 실패"));
        });
    }

    /* =========================================================
     * Fallbacks
     * ========================================================= */

    private CompletableFuture<RecipeCreateRequestDto> fallbackGenerateText(String s, String t, Throwable ex) {
        log.error("Gemini Text Fallback: {}", ex.getMessage());
        return CompletableFuture.failedFuture(new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "텍스트 분석 중 오류가 발생했습니다."));
    }

    private CompletableFuture<RecipeCreateRequestDto> fallbackGenerateUrl(String s, String u, String url, Throwable t) {
        log.error("Gemini URL Fallback: {}", t.getMessage());
        return CompletableFuture.failedFuture(new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "영상 분석 중 오류가 발생했습니다."));
    }
}