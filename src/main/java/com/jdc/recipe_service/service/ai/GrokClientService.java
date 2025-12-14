package com.jdc.recipe_service.service.ai;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.ai.RecipeAnalysisResponseDto;
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
    public CompletableFuture<RecipeCreateRequestDto> generateRecipeJson(String systemContent, String userContent) {
        log.info("Grok API 레시피 생성 호출");

        return callGrokApi(systemContent, userContent, 3000, 0.3)
                .flatMap(jsonString -> {
                    try {
                        String normalizedJson = normalizeFields(jsonString);

                        JsonNode rootNode = objectMapper.readTree(normalizedJson);
                        JsonNode targetNode = rootNode;

                        if (rootNode.has("service_response")) {
                            targetNode = rootNode.get("service_response");
                            log.debug("감지됨: wrapper 구조 (service_response 추출)");
                        }

                        RecipeCreateRequestDto recipe = objectMapper.treeToValue(targetNode, RecipeCreateRequestDto.class);

                        validateRecipeDto(recipe);
                        return Mono.just(recipe);
                    } catch (Exception e) {
                        log.error("DTO 파싱 실패. JSON: {}", jsonString);
                        return Mono.error(new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "JSON 파싱 실패: " + e.getMessage()));
                    }
                })
                .toFuture();
    }

    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerateRaw")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerateRaw")
    @TimeLimiter(name = "aiGenerate", fallbackMethod = "fallbackGenerateRaw")
    public CompletableFuture<String> generateRaw(String systemContent, String userContent) {
        log.info("Grok API Raw 호출");
        return callGrokApi(systemContent, userContent, 3000, 0.3)
                .map(jsonString -> {
                    try {
                        JsonNode rootNode = objectMapper.readTree(jsonString);
                        if (rootNode.has("service_response")) {
                            return rootNode.get("service_response").toString();
                        }
                        return jsonString;
                    } catch (Exception e) {
                        log.warn("Raw JSON 껍데기 제거 중 에러 (무시하고 원본 반환): {}", e.getMessage());
                        return jsonString;
                    }
                })
                .toFuture();
    }

    public CompletableFuture<RecipeAnalysisResponseDto> analyzeRecipe(String userPrompt) {
        log.info("Grok 레시피 분석 호출");

        String systemInstruction = "너는 JSON 응답만 출력하는 분석가야.";

        return callGrokApi(systemInstruction, userPrompt, 500, 0.1)
                .flatMap(jsonString -> {
                    try {
                        RecipeAnalysisResponseDto response = objectMapper.readValue(jsonString, RecipeAnalysisResponseDto.class);
                        return Mono.just(response);
                    } catch (Exception e) {
                        return Mono.error(new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "분석 결과 파싱 실패"));
                    }
                })
                .toFuture();
    }

    private Mono<String> callGrokApi(String systemContent, String userContent, int maxTokens, double temperature) {
        Map<String, Object> requestBody = Map.of(
                "model", grokRecipeModelName,
                "temperature", temperature,
                "max_tokens", maxTokens,
                "messages", List.of(
                        Map.of("role", "system", "content", systemContent),
                        Map.of("role", "user", "content", userContent)
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
                                    return Mono.error(new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Grok API 호출 실패"));
                                })
                )
                .bodyToMono(String.class)
                .timeout(Duration.ofSeconds(120))
                .doOnError(WebClientResponseException.class, e ->
                        log.error("WebClient 오류: status={}, body={}", e.getStatusCode(), e.getResponseBodyAsString())
                )
                .flatMap(this::extractContentString);
    }


    private Mono<String> extractContentString(String rawJsonResponse) {
        return Mono.fromCallable(() -> {
            if (rawJsonResponse == null || rawJsonResponse.trim().isEmpty()) {
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Grok API 응답이 비어 있습니다.");
            }
            try {
                Map<String, Object> responseMap = objectMapper.readValue(rawJsonResponse, new TypeReference<>() {});
                List<Map<String, Object>> choices = (List<Map<String, Object>>) responseMap.get("choices");

                if (choices == null || choices.isEmpty()) {
                    throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Grok API 응답에 choices가 없습니다.");
                }

                Map<String, Object> message = (Map<String, Object>) choices.get(0).get("message");
                String content = message.get("content").toString();

                log.debug("응답 content 앞 200자: {}", content.substring(0, Math.min(200, content.length())));

                return content.replaceAll("(?s)```json\\s*", "")
                        .replaceAll("(?s)```\\s*", "")
                        .trim();

            } catch (CustomException e) {
                throw e;
            } catch (Exception e) {
                log.error("JSON 추출 실패", e);
                throw new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "Grok 응답 처리 중 오류");
            }
        });
    }


    private CompletableFuture<RecipeCreateRequestDto> fallbackGenerate(String system, String user, Throwable ex) {
        log.error("Grok Fallback (DTO): {}", ex.getMessage());
        return CompletableFuture.failedFuture(new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI 생성 실패 (Fallback)"));
    }

    private CompletableFuture<String> fallbackGenerateRaw(String system, String user, Throwable ex) {
        log.error("Grok Fallback (Raw): {}", ex.getMessage());
        return CompletableFuture.failedFuture(new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "AI Raw 생성 실패 (Fallback)"));
    }


    private String normalizeFields(String json) {
        return json
                .replaceAll(
                        "\"(customPrice|customCalories|customCarbohydrate|customProtein|customFat|customSugar|customSodium|marketPrice|cookingTime|servings|protein|carbohydrate|fat|sugar|sodium)\"\\s*:\\s*(\"\\s*\"|null)",
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
            if (n.getProtein() != null && n.getProtein().compareTo(BigDecimal.ZERO) < 0)
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "단백질 음수");
            if (n.getCarbohydrate() != null && n.getCarbohydrate().compareTo(BigDecimal.ZERO) < 0)
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "탄수화물 음수");
            if (n.getFat() != null && n.getFat().compareTo(BigDecimal.ZERO) < 0)
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "지방 음수");
            if (n.getSugar() != null && n.getSugar().compareTo(BigDecimal.ZERO) < 0)
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "당류 음수");
            if (n.getSodium() != null && n.getSodium().compareTo(BigDecimal.ZERO) < 0)
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "나트륨 음수");
        }

        if (recipe.getCookingTime() != null && recipe.getCookingTime() < 0)
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "조리 시간 음수");
        if (recipe.getServings() != null && recipe.getServings() < 0)
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "인분 음수");
        if (recipe.getMarketPrice() != null && recipe.getMarketPrice() < 0)
            throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "시장 가격 음수");

        log.debug("레시피 DTO 검증 완료: title={}", recipe.getTitle());
    }
}