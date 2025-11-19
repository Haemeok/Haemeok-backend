package com.jdc.recipe_service.service.ai;

import com.fasterxml.jackson.core.type.TypeReference;
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

import java.math.BigDecimal;
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
        log.info(">>>> [USER PROMPT START] <<<<\n{}", prompt);

        Map<String, Object> requestBody = Map.of(
                "model", grokRecipeModelName,
                "temperature", 0.3,
                "max_tokens", 1500,
                "messages", List.of(
                        Map.of(
                                "role", "system",
                                "content", "너는 한국요리 전문가야. 응답은 오직 JSON 객체 형태여야 하며, 추가 텍스트 금지. 요청 조건 재료만 100% 사용. 모든 필드 한글 표기."
                        ),
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
                .flatMap(this::parseGrokResponse)
                .toFuture();
    }

    private Mono<RecipeCreateRequestDto> parseGrokResponse(String jsonResponse) {
        return Mono.fromCallable(() -> {
            if (jsonResponse == null || jsonResponse.trim().isEmpty()) {
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Grok API 응답이 비어 있습니다.");
            }

            String cleanedJson = null;

            try {
                Map<String, Object> responseMap = objectMapper.readValue(jsonResponse, new TypeReference<Map<String, Object>>() {});
                List<Map<String, Object>> choices = (List<Map<String, Object>>) responseMap.get("choices");

                if (choices == null || choices.isEmpty()) {
                    log.error("choices 배열이 비어있음. 전체 응답: {}", jsonResponse);
                    throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Grok API 응답에 choices가 없습니다.");
                }

                Map<String, Object> firstChoice = choices.get(0);
                Map<String, Object> message = (Map<String, Object>) firstChoice.get("message");

                if (message == null || message.get("content") == null) {
                    throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Grok API 응답 message가 없습니다.");
                }

                String content = message.get("content").toString();
                log.debug("응답 content 길이: {}, 앞 200자: {}", content.length(), content.substring(0, Math.min(200, content.length())));

                cleanedJson = cleanJsonResponse(content);
                cleanedJson = normalizeFields(cleanedJson);

                log.info(">>>> [AI GENERATED RECIPE JSON START] <<<<\n{}", cleanedJson);
                log.info(">>>> [AI GENERATED RECIPE JSON END] <<<<");

                RecipeCreateRequestDto recipe = objectMapper.readValue(cleanedJson, RecipeCreateRequestDto.class);
                validateRecipeDto(recipe);
                log.info("레시피 파싱 성공: title={}", recipe.getTitle());
                return recipe;

            } catch (CustomException e) {
                throw e;
            } catch (Exception e) {
                log.error("JSON 파싱 실패: {}", e.getMessage(), e);
                log.error("🚨 Conversion 오류 유발 JSON (전체): \n{}", cleanedJson);
                throw new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "Grok 응답 JSON 파싱 실패: " + e.getMessage(), e);
            }
        });
    }

    private String cleanJsonResponse(String content) {
        return content.replaceAll("(?s)```json\\s*", "")
                .replaceAll("(?s)```\\s*", "")
                .trim();
    }

    private String normalizeFields(String json) {
        return json
                .replaceAll(
                        "\"(customPrice|caloriesPerUnit|marketPrice|cookingTime|servings|protein|carbohydrate|fat|sugar|sodium)\"\\s*:\\s*(\"\\s*\"|null)",
                        "\"$1\": 0"
                )
                .replaceAll("\"quantity\"\\s*:\\s*(\"\\s*\"|null)", "\"quantity\": \"0\"")
                .replaceAll("\"dishType\"\\s*:\\s*(\"\\s*\"|null)", "\"dishType\": \"기타\"");
    }

    private void validateRecipeDto(RecipeCreateRequestDto recipe) {
        if (recipe.getDishType() == null || recipe.getDishType().trim().isEmpty()) {
            recipe.setDishType("기타");
            log.warn("dishType이 비어있어 '기타'로 설정됨");
        }

        if (recipe.getIngredients() != null) {
            for (var ing : recipe.getIngredients()) {
                if (ing.getCustomPrice() != null && ing.getCustomPrice().compareTo(BigDecimal.ZERO) < 0) {
                    throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "재료 가격이 음수: " + ing.getName());
                }
                if (ing.getCustomCalories() != null && ing.getCustomCalories().compareTo(BigDecimal.ZERO) < 0) {
                    throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "재료 칼로리가 음수: " + ing.getName());
                }
            }
        }

        if (recipe.getNutrition() != null) {
            var n = recipe.getNutrition();
            if (n.getProtein() != null && n.getProtein().compareTo(BigDecimal.ZERO) < 0) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "단백질 음수");
            if (n.getCarbohydrate() != null && n.getCarbohydrate().compareTo(BigDecimal.ZERO) < 0) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "탄수화물 음수");
            if (n.getFat() != null && n.getFat().compareTo(BigDecimal.ZERO) < 0) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "지방 음수");
            if (n.getSugar() != null && n.getSugar().compareTo(BigDecimal.ZERO) < 0) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "당류 음수");
            if (n.getSodium() != null && n.getSodium() < 0) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "나트륨 음수");
        }

        if (recipe.getCookingTime() != null && recipe.getCookingTime() < 0) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "조리 시간 음수");
        if (recipe.getServings() != null && recipe.getServings() < 0) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "인분 음수");
        if (recipe.getMarketPrice() != null && recipe.getMarketPrice() < 0) throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "시장 가격 음수");

        log.debug("레시피 DTO 검증 완료: title={}", recipe.getTitle());
    }

    private CompletableFuture<RecipeCreateRequestDto> fallbackGenerate(String prompt, Throwable ex) {
        log.error("Grok fallback 실행: {}", ex.getMessage(), ex);
        return CompletableFuture.failedFuture(
                new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Grok 레시피 생성 실패 (재시도/서킷/타임아웃): " + ex.getMessage(),
                        ex)
        );
    }
}
