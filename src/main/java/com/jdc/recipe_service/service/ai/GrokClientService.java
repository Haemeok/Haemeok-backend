package com.jdc.recipe_service.service.ai;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.ai.RecipeAnalysisResponseDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
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
import java.util.Collections;
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

    @Value("${ai.model.grok.recipe:grok-4-1-fast-reasoning}")
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

    @Retry(name = "aiGenerate", fallbackMethod = "fallbackRefineIngredients")
    public CompletableFuture<List<RecipeIngredientRequestDto>> refineIngredientsOnly(String systemContent, List<RecipeIngredientRequestDto> rawIngredients) {
        log.info("🤖 Grok 2단계: 재료 부분 최적화 시작 (입력 개수: {})", rawIngredients.size());
        try {
            List<String> allNames = rawIngredients.stream().map(i -> i.getName().trim()).toList();
            List<Ingredient> dbIngredients = ingredientRepository.findAllByNameIn(allNames);
            Map<String, String> dbUnitMap = dbIngredients.stream()
                    .collect(Collectors.toMap(Ingredient::getName, Ingredient::getUnit, (a, b) -> a));

            StringBuilder ingredientReport = new StringBuilder();
            for (var ing : rawIngredients) {
                String name = ing.getName().trim();
                String dbUnit = dbUnitMap.get(name);
                if (dbUnit != null) {
                    ingredientReport.append(String.format("- [DB보유] '%s': 표준 단위 '%s'로 변환.\n", name, dbUnit));
                } else {
                    ingredientReport.append(String.format("- [미보유] '%s': 영양정보 생성 대상.\n", name));
                }
            }

            String ingredientsJson = objectMapper.writeValueAsString(rawIngredients);

            String userContent = """
                    너는 '식재료 데이터 규격화 전문가'다.
                    입력된 **재료 리스트(JSON Array)**를 분석 보고서에 따라 수정해라.
                    
                    [🚨 재료 분석 보고서 (Java 시스템 분석 결과)]
                    %s
                    
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
                       - **수량(quantity)은 소수점 둘째 자리 이상 나올 경우, 첫째 자리에서 반올림하여 인간이 읽기 편한 숫자(예: 0.5, 1.5, 10 등)로 변환하세요.**
                       - 모든 재료의 quantity는 요청된 인분 수에 맞추어 자동으로 조절되어야 합니다.
                    
                    [입력 JSON]
                    %s
                    
                    [출력 형식]
                    1. 반드시 **JSON Array** (`[...]`) 포맷으로 출력해라.
                    2. 절대 단일 객체(`{...}`)로 감싸거나 병합하지 마라.
                    3. 예시: [ {"name": "...", ...}, {"name": "...", ...} ]
                    4. 응답의 시작은 반드시 '[' 문자여야 하고, 끝은 ']' 여야 한다.
                    """.formatted(ingredientReport.toString(), ingredientsJson);

            return callGrokApi(systemContent, userContent, 5000, 0.2)
                    .flatMap(jsonString -> {
                        try {
                            String fixedJson = repairMalformedJson(jsonString);

                            JsonNode rootNode = objectMapper.readTree(fixedJson);

                            if (rootNode.isArray()) {
                                return convertToList(rootNode);
                            }

                            if (rootNode.isObject()) {
                                if (rootNode.has("ingredients") && rootNode.get("ingredients").isArray()) {
                                    return convertToList(rootNode.get("ingredients"));
                                }
                                if (rootNode.has("service_response") && rootNode.get("service_response").isArray()) {
                                    return convertToList(rootNode.get("service_response"));
                                }
                                for (JsonNode child : rootNode) {
                                    if (child.isArray() && !child.isEmpty()) {
                                        return convertToList(child);
                                    }
                                }
                            }

                            throw new RuntimeException("JSON Array not found");

                        } catch (Exception e) {
                            log.error("재료 파싱 실패. 원본: {}", jsonString);
                            return Mono.just(rawIngredients);
                        }
                    })
                    .toFuture();

        } catch (Exception e) {
            return CompletableFuture.completedFuture(rawIngredients);
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

    public CompletableFuture<List<String>> filterRecipeVideos(List<Map<String, String>> candidates) {
        if (candidates.isEmpty()) {
            return CompletableFuture.completedFuture(Collections.emptyList());
        }

        try {
            String candidatesJson = objectMapper.writeValueAsString(candidates);

            String systemPrompt = """
                당신은 '유튜브 요리 레시피 분류기'입니다.
                입력된 영상 목록(JSON)에서 '요리 레시피(Recipe) 영상'의 ID만 골라내세요.
                
                [판단 기준]
                1. Title: '만드는 법', '레시피', '하기', '밥상' 등 조리 의도 포함.
                2. Channel: 'Mukbang', 'Vlog' 등은 배제.
                
                [제외 대상]
                - 맛집 탐방, 먹방, 일상 브이로그, 편의점 꿀조합
                
                [출력 형식 - 중요]
                - JSON 객체 포맷을 준수해야 합니다.
                - 레시피 ID들의 리스트를 "ids"라는 키에 담아 출력하세요.
                - 예시: { "ids": ["videoId1", "videoId2", "videoId3"] }
                """;

            String userPrompt = "분석 대상 목록: " + candidatesJson;

            return callGrokApi(systemPrompt, userPrompt, 2000, 0.1)
                    .flatMap(jsonString -> {
                        try {
                            JsonNode rootNode = objectMapper.readTree(jsonString);
                            JsonNode idsNode = rootNode.get("ids");

                            List<String> validIds;
                            if (idsNode != null && idsNode.isArray()) {
                                validIds = objectMapper.convertValue(idsNode, new TypeReference<List<String>>() {});
                            } else {
                                validIds = Collections.emptyList();
                            }

                            log.info("🎯 AI 필터링 결과: 입력 {}개 -> 통과 {}개", candidates.size(), validIds.size());
                            return Mono.just(validIds);
                        } catch (Exception e) {
                            log.error("AI 응답 파싱 실패. JSON: {}", jsonString, e);
                            return Mono.just(Collections.<String>emptyList());
                        }
                    })
                    .toFuture();

        } catch (Exception e) {
            log.error("필터링 요청 생성 중 에러", e);
            return CompletableFuture.completedFuture(Collections.emptyList());
        }
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

    private String repairMalformedJson(String json) {
        String trimmed = json.trim();
        if (trimmed.startsWith("[")) return trimmed;

        if (trimmed.startsWith("{") && trimmed.contains("\"name\"")) {
            String repaired = trimmed.replaceAll(",\\s*\"name\"\\s*:", "},{\"name\":");
            return "[" + repaired + "]";
        }
        return json;
    }

    private Mono<List<RecipeIngredientRequestDto>> convertToList(JsonNode node) {
        List<RecipeIngredientRequestDto> refinedList = objectMapper.convertValue(
                node,
                new TypeReference<List<RecipeIngredientRequestDto>>() {}
        );
        return Mono.just(refinedList);
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

                Object usageObj = responseMap.get("usage");
                if (usageObj instanceof Map) {
                    @SuppressWarnings("unchecked")
                    Map<String, Object> usage = (Map<String, Object>) usageObj;

                    Integer promptTokens = usage.get("prompt_tokens") instanceof Number
                            ? ((Number) usage.get("prompt_tokens")).intValue() : 0;

                    Integer cachedTokens = 0;
                    Object detailsObj = usage.get("prompt_tokens_details");
                    if (detailsObj instanceof Map) {
                        @SuppressWarnings("unchecked")
                        Map<String, Object> details = (Map<String, Object>) detailsObj;
                        cachedTokens = details.get("cached_tokens") instanceof Number
                                ? ((Number) details.get("cached_tokens")).intValue() : 0;
                    }

                    log.info("Grok API Usage - total_prompt: {}, cached_prompt: {} (hit rate: {}%)",
                            promptTokens, cachedTokens,
                            promptTokens > 0 ? Math.round((double) cachedTokens / promptTokens * 100) : 0);
                } else {
                    log.warn("Grok API 응답에 usage 객체가 없거나 예상 형식과 다릅니다.");
                }

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

    private CompletableFuture<List<RecipeIngredientRequestDto>> fallbackRefineIngredients(String s, List<RecipeIngredientRequestDto> raw, Throwable t) {
        log.error("재료 정제 실패(Fallback): {}", t.getMessage());
        return CompletableFuture.completedFuture(raw);
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
                        "\"(marketPrice|cookingTime|servings|protein|carbohydrate|fat|sugar|sodium)\"\\s*:\\s*(\"\\s*\"|null)",
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