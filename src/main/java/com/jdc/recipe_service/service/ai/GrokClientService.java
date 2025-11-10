package com.jdc.recipe_service.service.ai;

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
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Mono;
import java.time.Duration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Slf4j
public class GrokClientService {

    @Qualifier("grokWebClient")
    private final WebClient client;
    private final ObjectMapper objectMapper;

    @Value("${ai.model.grok.recipe:grok-4-fast-reasoning}")
    private String grokRecipeModelName;

    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @TimeLimiter(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    public CompletableFuture<RecipeCreateRequestDto> generateRecipeJson(String prompt) {
        log.info("Grok API 호출 시작: model={}", grokRecipeModelName);

        Map<String, Object> requestBody = Map.of(
                "model", grokRecipeModelName,
                "temperature", 0.0,
                "max_tokens", 1500,
                "messages", List.of(
                        Map.of("role", "system", "content", "너는 한국요리 전문가야. 응답은 오직 JSON 객체 형태여야 하며, 어떤 설명, 주석, 마커, 추가 텍스트도 포함해서는 안 돼."),
                        Map.of("role", "user", "content", prompt)
                ),
                "response_format", Map.of("type", "json_object")
        );

        return client.post()
                .uri("/chat/completions")
                .bodyValue(requestBody)
                .retrieve()
                .onStatus(
                        status -> status.is4xxClientError() || status.is5xxServerError(),
                        response -> response.bodyToMono(String.class)
                                .flatMap(body -> {
                                    log.error("Grok API 오류: Status={}, Body={}", response.statusCode(), body);
                                    return Mono.error(new CustomException(
                                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                                            "Grok API 호출 실패: " + response.statusCode() + " - " + body
                                    ));
                                })
                )
                .bodyToMono(String.class)
                .timeout(Duration.ofSeconds(120))
                .doOnError(WebClientResponseException.class, e ->
                        log.error("WebClient 오류: status={}, body={}", e.getStatusCode(), e.getResponseBodyAsString())
                )
                .flatMap(jsonResponse -> {
                    log.debug("Grok API 응답 수신 (길이: {})", jsonResponse.length());
                    return parseGrokResponse(jsonResponse);
                })
                .toFuture();
    }

    private Mono<RecipeCreateRequestDto> parseGrokResponse(String jsonResponse) {
        return Mono.fromCallable(() -> {
            if (jsonResponse == null || jsonResponse.trim().isEmpty()) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Grok API 응답이 비어 있습니다."
                );
            }

            try {
                Map<String, Object> responseMap = objectMapper.readValue(jsonResponse, Map.class);
                log.debug("응답 맵 키: {}", responseMap.keySet());

                List<Map<String, Object>> choices = (List<Map<String, Object>>) responseMap.get("choices");
                if (choices == null || choices.isEmpty()) {
                    log.error("choices 배열이 비어있음. 전체 응답: {}", jsonResponse);
                    throw new CustomException(
                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                            "Grok API 응답에 choices가 없습니다."
                    );
                }

                Map<String, Object> firstChoice = choices.get(0);
                Map<String, Object> message = (Map<String, Object>) firstChoice.get("message");
                if (message == null) {
                    throw new CustomException(
                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                            "Grok API 응답에 message가 없습니다."
                    );
                }

                String content = (String) message.get("content");
                if (content == null || content.trim().isEmpty()) {
                    throw new CustomException(
                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                            "Grok API 응답 content가 비어 있습니다."
                    );
                }

                log.debug("추출된 content 길이: {}, 내용(앞 200자): {}",
                        content.length(),
                        content.substring(0, Math.min(200, content.length()))
                );

                String cleanedJson = cleanJsonResponse(content);

                RecipeCreateRequestDto recipe = objectMapper.readValue(
                        cleanedJson,
                        RecipeCreateRequestDto.class
                );

                log.info("레시피 파싱 성공: title={}", recipe.getTitle());
                return recipe;

            } catch (CustomException e) {
                throw e;
            } catch (Exception e) {
                String snippet = jsonResponse.length() > 300
                        ? jsonResponse.substring(0, 300) + "..."
                        : jsonResponse;
                log.error("JSON 파싱 실패. 원본(일부): {}", snippet, e);
                throw new CustomException(
                        ErrorCode.INTERNAL_SERVER_ERROR,
                        "Grok 응답 JSON 파싱 실패: " + e.getMessage(),
                        e
                );
            }
        });
    }

    /**
     * Grok이 반환한 JSON에서 불필요한 마크다운, 공백 등을 제거
     */
    private String cleanJsonResponse(String content) {
        content = content.replaceAll("(?s)```json\\s*", "")
                .replaceAll("(?s)```\\s*", "")
                .trim();

        content = content.trim();

        return content;
    }

    private CompletableFuture<RecipeCreateRequestDto> fallbackGenerate(String prompt, Throwable ex) {
        log.error("Grok fallback 실행: {}", ex.getMessage(), ex);
        return CompletableFuture.failedFuture(
                new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Grok 레시피 생성 실패 (재시도/서킷/타임아웃): " + ex.getMessage(),
                        ex
                )
        );
    }
}