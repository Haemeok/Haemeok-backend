package com.jdc.recipe_service.service.ai;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.ai.RecipeAnalysisResponseDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
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
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class GrokClientService {

    @Qualifier("grokWebClient")
    private final WebClient client;
    private final ObjectMapper objectMapper;
    private final IngredientRepository ingredientRepository;

    @Value("${ai.model.grok.recipe:grok-4-fast-reasoning}")
    private String grokRecipeModelName;

    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @TimeLimiter(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    public CompletableFuture<RecipeCreateRequestDto> generateRecipeStep1(String systemContent, String fullContext) {
        log.info("Grok 1단계: 자연스러운 레시피 생성 호출");

        String userContent = """
                다음은 요리 영상의 제목, 설명, 댓글, 자막입니다.
                이를 분석해서 맛있고 자연스러운 레시피를 만들어줘.
                
                입력:
                %s
                """.formatted(fullContext);

        return generateRecipeJson(systemContent, userContent);
    }

    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @TimeLimiter(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    public CompletableFuture<RecipeCreateRequestDto> refineRecipeToStandard(String systemContent, RecipeCreateRequestDto rawRecipe) {
        log.info("🤖 Grok 2단계: 재료 규격화 및 커스텀 데이터 생성 시작");

        try {
            List<String> allNames = rawRecipe.getIngredients().stream()
                    .map(ing -> ing.getName().trim())
                    .toList();

            List<Ingredient> dbIngredients = ingredientRepository.findAllByNameIn(allNames);

            Map<String, String> dbUnitMap = dbIngredients.stream()
                    .collect(Collectors.toMap(
                            Ingredient::getName,
                            Ingredient::getUnit,
                            (existing, replacement) -> existing
                    ));

            String rawJson = objectMapper.writeValueAsString(rawRecipe);
            StringBuilder ingredientReport = new StringBuilder();

            for (var ing : rawRecipe.getIngredients()) {
                String name = ing.getName().trim();

                String dbUnit = dbUnitMap.get(name);

                if (dbUnit != null) {
                    ingredientReport.append(String.format(
                            "- [DB보유] '%s': 표준 단위 '%s'로 환산. custom 필드 삭제 대상.\n",
                            name, dbUnit
                    ));
                } else {
                    ingredientReport.append(String.format(
                            "- [미보유/신규] '%s': 현재 수량(%s %s) 기준. 아래 7개 상세 영양 정보 필수.\n",
                            name, ing.getQuantity(), ing.getCustomUnit()
                    ));
                }
            }

            String userContent = """
                    너는 '데이터 규격화 전문가'다.
                    1단계 JSON을 입력받아, 아래 **[재료 분석 보고서]**를 기준으로 **[필드 강제 규칙]**을 100%% 준수하여 재료(ingredients) 필드를 완벽하게 수정해라.
                    
                    [🚨 재료 분석 보고서 (Java 시스템 분석 결과)]
                    %s
                    
                    [🚨 CRITICAL WARNING: 숫자 필드 NULL/공백 절대 금지]
                    - **모든 숫자 필드** `quantity`, `customPrice`, `customCalories`, `customCarbohydrate`, `customProtein`, `customFat`, `customSugar`, `customSodium`, `marketPrice`, `cookingTime`는 **0.00 이상의 유효한 숫자만** 허용됩니다.
                    - **`servings`(인분)는 반드시 '정수(Integer)'로 반올림하여 출력하세요.**
                    - **절대로 빈 문자열("") 또는 null 값을 사용하지 마세요.**
                    
                    [🚨 ingredients 필드 강제 규칙 - 반드시 준수]
                    1. **[미보유/신규] 재료의 경우**:
                       DB에 없는 재료이므로 **반드시** 아래 7개 필드를 모두 포함해야 합니다:
                       - `customPrice`: **해당 재료의 Quantity(총량)에 대한 전체 원가** (정수, 원).
                       - `customCalories`: **해당 재료의 Quantity(총량)에 대한 전체 칼로리** (소수점 포함 숫자, kcal)
                       - `customCarbohydrate`: **해당 재료의 Quantity(총량)에 대한 전체 탄수화물** (소수점 포함 숫자, g)
                       - `customProtein`: **해당 재료의 Quantity(총량)에 대한 전체 단백질** (소수점 포함 숫자, g)
                       - `customFat`: **해당 재료의 Quantity(총량)에 대한 전체 지방** (소수점 포함 숫자, g)
                       - `customSugar`: **해당 재료의 Quantity(총량)에 대한 전체 당류** (소수점 포함 숫자, g)
                       - `customSodium`: **해당 재료의 Quantity(총량)에 대한 전체 나트륨** (소수점 포함 숫자, mg)
                       - **이 필드 중 하나라도 누락되면 출력 전체가 무효 처리됩니다.**
                    
                    2. **[DB보유] 재료의 경우**:
                       - `customPrice`, `customCalories`, `customCarbohydrate`, `customProtein`, `customFat`, `customSugar`, `customSodium` 필드는 **절대 포함 금지** (반드시 제거하거나 null 처리).
                       - 단위(`unit`)는 보고서에 적힌 '표준 단위'로 수정하세요.

                    3. **공통 수량 규칙**:
                       - "반 개", "한 줌" 같은 텍스트는 "0.5", "30" 같은 **숫자**로 무조건 변환하세요.
                       - 모든 재료의 quantity는 요청된 인분 수에 맞추어 자동으로 조절되어야 합니다.
                    
                    [입력 JSON]
                    %s
                    
                    다른 필드(steps, description 등)는 원본을 유지하고, 오직 수정된 JSON만 출력해라.
                    """.formatted(ingredientReport.toString(), rawJson);

            return generateRecipeJson(systemContent, userContent);

        } catch (Exception e) {
            log.error("2단계 정제 중 에러: {}", e.getMessage());
            return CompletableFuture.completedFuture(rawRecipe);
        }
    }

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

                        log.info("=== Grok 레시피 생성 성공 ===");
                        log.info("Title: {}", recipe.getTitle());
                        log.info("DishType: {}", recipe.getDishType());
                        log.info("Servings: {}", recipe.getServings());
                        log.info("CookingTime: {}분", recipe.getCookingTime());
                        log.info("Ingredients: {}개, Steps: {}단계",
                                recipe.getIngredients() == null ? 0 : recipe.getIngredients().size(),
                                recipe.getSteps() == null ? 0 : recipe.getSteps().size());

                        if (log.isDebugEnabled() && recipe.getIngredients() != null) {
                            recipe.getIngredients().forEach(ing -> {
                                log.debug("  → {} | {} {}", ing.getName(), ing.getQuantity(), ing.getCustomUnit());
                            });
                        }

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

    public CompletableFuture<RecipeCreateRequestDto> fallbackGenerate(String systemContent, RecipeCreateRequestDto rawRecipe, Throwable t) {
        log.error("Grok 2단계 정제 실패 (Fallback): {}", t.getMessage());
        return CompletableFuture.completedFuture(rawRecipe);
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