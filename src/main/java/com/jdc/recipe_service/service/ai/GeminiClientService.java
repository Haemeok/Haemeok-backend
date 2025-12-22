package com.jdc.recipe_service.service.ai;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import io.github.resilience4j.timelimiter.annotation.TimeLimiter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@Service
@Slf4j
public class GeminiClientService {

    private final WebClient webClient;
    private final ObjectMapper objectMapper;

    @Value("${gemini.api-key}")
    private String geminiApiKey;

    private static final String GCP_PROJECT_ID = "gen-lang-client-0326396795";
    private static final String GEMINI_MODEL_ID = "gemini-3-flash-preview";

    private static final String GEMINI_API_URL =
            "https://aiplatform.googleapis.com/v1/projects/" + GCP_PROJECT_ID +
                    "/locations/global/publishers/google/models/" + GEMINI_MODEL_ID + ":generateContent";

    public GeminiClientService(ObjectMapper objectMapper) {
        this.webClient = WebClient.builder().build();
        this.objectMapper = objectMapper;
    }
    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @TimeLimiter(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    public CompletableFuture<RecipeCreateRequestDto> generateRecipeJson(String systemContent, String userContent) {
        log.info("[GeminiClientService] 파인다이닝 레시피 생성 시작");

        return callGeminiApi(systemContent, userContent)
                .flatMap(rawResponse -> {
                    try {
                        int start = rawResponse.indexOf("{");
                        int end = rawResponse.lastIndexOf("}");

                        if (start == -1 || end == -1) {
                            throw new RuntimeException("유효한 JSON 형식을 찾을 수 없습니다.");
                        }

                        String cleanedJson = rawResponse.substring(start, end + 1).trim();
                        log.debug("정제된 JSON: {}", cleanedJson);

                        RecipeCreateRequestDto recipe = objectMapper.readValue(cleanedJson, RecipeCreateRequestDto.class);
                        return Mono.just(recipe);

                    } catch (Exception e) {
                        try {
                            log.warn("단일 객체 파싱 실패, 리스트 형태 파싱 시도...");
                            int startArr = rawResponse.indexOf("[");
                            int endArr = rawResponse.lastIndexOf("]");

                            if (startArr != -1 && endArr != -1) {
                                String listJson = rawResponse.substring(startArr, endArr + 1).trim();
                                List<RecipeCreateRequestDto> list = objectMapper.readValue(listJson,
                                        new com.fasterxml.jackson.core.type.TypeReference<List<RecipeCreateRequestDto>>() {});

                                if (!list.isEmpty()) {
                                    return Mono.just(list.get(0));
                                }
                            }
                        } catch (Exception ignore) {}

                        log.error("Gemini 응답 최종 파싱 실패: {}", rawResponse, e);
                        return Mono.error(new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "AI 응답 처리 오류"));
                    }
                })
                .toFuture();
    }

    private Mono<String> callGeminiApi(String systemContent, String userContent) {
        Map<String, Object> requestBody = Map.of(
                "system_instruction", Map.of("parts", List.of(Map.of("text", systemContent))),
                "contents", List.of(Map.of("role", "user", "parts", List.of(Map.of("text", userContent)))),
                "generationConfig", Map.of(
                        "responseMimeType", "application/json",
                        "temperature", 0.7,
                        "maxOutputTokens", 10000,
                        "thinking_config", Map.of("thinking_level", "HIGH")
                )
        );

        return webClient.post()
                .uri(GEMINI_API_URL + "?key=" + geminiApiKey)
                .headers(headers -> headers.remove("Authorization"))
                .bodyValue(requestBody)
                .retrieve()
                .onStatus(status -> status.isError(), response ->
                        response.bodyToMono(String.class).flatMap(errorBody -> {
                            log.error("Gemini API 호출 에러: {}", errorBody);
                            return Mono.error(new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Gemini API 통신 실패"));
                        })
                )
                .bodyToMono(JsonNode.class)
                .timeout(Duration.ofSeconds(180))
                .map(rootNode -> {
                    return rootNode.path("candidates").get(0)
                            .path("content").path("parts").get(0)
                            .path("text").asText();
                });
    }

    private CompletableFuture<RecipeCreateRequestDto> fallbackGenerate(String system, String user, Throwable ex) {
        log.error("Gemini Fallback: {}", ex.getMessage());
        return CompletableFuture.failedFuture(new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "파인다이닝 생성 중 일시적인 오류 발생"));
    }
}