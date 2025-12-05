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
import java.util.ArrayList;
import java.util.HashMap;
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
                "max_tokens", 3000,
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

    public CompletableFuture<RecipeAnalysisResponseDto> analyzeRecipe(String prompt) {
        log.info("Grok 레시피 분석 호출 시작");

        Map<String, Object> requestBody = Map.of(
                "model", grokRecipeModelName,
                "temperature", 0.1,
                "max_tokens", 500,
                "messages", List.of(
                        Map.of("role", "system", "content", "너는 JSON 응답만 출력하는 분석가야."),
                        Map.of("role", "user", "content", prompt)
                ),
                "response_format", Map.of("type", "json_object")
        );

        return client.post()
                .uri("/chat/completions")
                .bodyValue(requestBody)
                .retrieve()
                .bodyToMono(String.class)
                .map(this::parseAnalysisResponse)
                .toFuture();
    }

    @TimeLimiter(name = "grok")
    @CircuitBreaker(name = "grok", fallbackMethod = "fallbackFinalPrompt")
    @Retry(name = "grok")
    public CompletableFuture<String> generateFinalImagePrompt(String title, String ingredients, String dishType, String description) {
        log.info("Grok에게 전체 프롬프트 작성 요청: {}", title);

        Map<String, Object> requestBody = buildFullPromptRequestBody(title, ingredients, dishType, description);

        return client.post()
                .uri("/chat/completions")
                .bodyValue(requestBody)
                .retrieve()
                .bodyToMono(String.class)
                .map(this::parseDescriptionContent)
                .toFuture();
    }

    private Map<String, Object> buildFullPromptRequestBody(String title, String ingredients, String dishType, String description) {

        String systemPrompt = """
                You are an expert AI Prompt Engineer specialized in High-End Food Photography.
                Your goal is to analyze the input recipe and write a ONE SINGLE, HIGH-QUALITY PROMPT for Google Imagen.
                
                [UNIVERSAL LOGIC - Apply to ANY Dish]
                
                1. **The "Edibility" Rule (State Transformation):**
                   - **Raw Meat/Seafood:** Must be described as **"Fully Cooked"**, **"Braised"**, **"Grilled"**, or **"Fried"**. NEVER raw unless specified (e.g., Sashimi).
                   - **Inedible Parts:** AUTOMATICALLY REMOVE shells, skins, tails, bones, and stems. (e.g., Shrimp -> "Peeled shrimp", Clam -> "Shelled clam", Egg -> "Cooked egg/Yolk").
                   - **Processing:** If ingredient is 'Minced' or 'Chopped', describe it as "integrated particles" or "sauce texture", NOT as whole objects.
                
                2. **The "Color & Consistency" Rule:**
                   - Derive the color ONLY from the provided sauces/spices (e.g., Chili=Red, Cream=White, Soy=Dark Brown).
                   - Determine liquid consistency based on the **Dish Category**:
                     * Soup/Stew -> Liquid/Broth (Translucent or Thick based on ingredients).
                     * Stir-fry/Roast -> Glossy Glaze/Coated.
                     * Salad/Cold -> Fresh/Dry.
                
                3. **The "Zero Hallucination" Rule (Strict):**
                   - **ONLY** use ingredients listed in the input.
                   - DO NOT add unlisted garnishes (No Sesame, No Parsley, No Scallions unless listed).
                   - DO NOT add side dishes.
                
                4. **The "Focus" Rule:**
                   - Filter out invisible ingredients (Salt, Sugar, Water, Vinegar, Oil, MSG).
                   - Focus description on the **Main Solids** (Meat, Veggies, Tofu, Noodles).
                
                    [STYLE GUIDE - Select the best option based on the dish]
                    - **Lighting:** 1. **Soft Window Light:** (Bright, airy, natural. Best for breakfast/lunch).
                       2. **Warm Ambient Light:** (Cozy, appetizing home-cooking vibe. Best for stews/rice).
                       3. **Cinematic Lighting:** (High contrast, dramatic shadows. Best for premium dinner/steak).
               
                    - **Angle:** 1. **45-degree (Standard):** (Best for Bowls, Soups, Deep plates).
                       2. **Top-down (Flat lay):** (Best for Spreads, Pizza, Bibimbap, Platter).
                       3. **Eye-level (Side view):** (Best for Burgers, Sandwiches, Stacked desserts).
                       4. **Macro (Close-up):** (Best for highlighting textures like grains or sauce).
                
                    - **Background:** 1. **Clean Wooden Table:** (Warm, rustic).
                       2. **White Marble Surface:** (Modern, clean).
                       3. **Blurred Dining Room:** (Depth, realistic).
                
                [TASK]
                Fill in the prompt template. Output ONLY the filled text.
                """;

        String userPrompt = String.format("""
                **Recipe Info:**
                - Title: %s
                - Category: %s
                - Description: %s
                - Ingredients: %s
                
                **REQUIRED OUTPUT FORMAT (Fill in the brackets):**
                
                **[Subject]**
                A high-resolution, appetizing food photography of "%s".
                Category: %s.
                
                **[Visual Contents]**
                - **Key Ingredients:** (Write a descriptive paragraph here. Describe colors/textures. e.g. "Chunks of pork and translucent kimchi in a pale orange broth...")
                - **Overall Vibe:** The food looks freshly cooked, appetizing, and high-quality.
                
                **[Visual Details & Texture]**
                - **Texture:** (Choose best adjectives from Style Guide e.g., Moist, Glossy)
                - **Sauce & Glaze:** (Describe color/consistency e.g., Clear golden broth)
                
                **[Composition & Styling]**
                - **Plating:** Served in a clean porcelain bowl.
                - **Framing:** Medium shot (Zoom out slightly).
                - **Angle:** (Choose best angle from Style Guide)
                - **Lighting:** (Choose best lighting from Style Guide)
                - **Background:** Clean wooden table. NO side dishes.
                
                **[Technical Quality]**
                - 8k resolution, hyper-realistic, cinematic lighting
                - Shallow depth of field (smooth bokeh)
                - Michelin star plating aesthetic
                --no text, --no watermark, --no hands, --no messy piles,
                --no raw powder, --no distorted blurry food, --no cropped plate,
                --no raw granules, --no visible dry seasoning, --no scattered toppings,
                --no mutated, --no irrational geometry, --no unnatural texture, --no plastic look, --no bad anatomy.
                
                (OUTPUT ONLY THE FILLED PROMPT TEXT. NO EXPLANATION.)
                """, title, dishType, description, ingredients, title, dishType);

        List<Map<String, String>> messages = new ArrayList<>();
        messages.add(Map.of("role", "system", "content", systemPrompt));
        messages.add(Map.of("role", "user", "content", userPrompt));

        Map<String, Object> body = new HashMap<>();
        body.put("model", grokRecipeModelName);
        body.put("messages", messages);
        body.put("temperature", 0.3);
        body.put("stream", false);

        return body;
    }

    private String parseDescriptionContent(String jsonResponse) {
        try {
            JsonNode root = objectMapper.readTree(jsonResponse);
            return root.path("choices")
                    .get(0)
                    .path("message")
                    .path("content")
                    .asText();
        } catch (Exception e) {
            log.error("Grok 이미지 묘사 응답 파싱 실패: {}", jsonResponse, e);
            throw new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "AI 응답 파싱 중 오류 발생");
        }
    }

    private CompletableFuture<String> fallbackFinalPrompt(String title, String ingredients, String dishType, String description, Throwable ex) {
        log.error("Grok 프롬프트 생성 실패 (Fallback 실행): {}", ex.getMessage());

        // Grok이 죽었을 때를 대비한 비상용 단순 프롬프트 (Java 포맷팅으로 떼우기)
        String fallbackPrompt = String.format(
                "**[Subject]**\n" +
                        "A high-resolution, appetizing food photography of \"%s\".\n" +
                        "Category: %s.\n\n" +
                        "**[Visual Contents]**\n" +
                        "- **Key Ingredients:** %s\n" +
                        "- **Overall Vibe:** Delicious and high quality.\n\n" +
                        "**[Technical Quality]**\n" +
                        "- 8k resolution, hyper-realistic, cinematic lighting.\n" +
                        "--no text, --no watermark, --no messy piles, --no shells, --no raw meat.",
                title,
                dishType != null ? dishType : "Food",
                ingredients
        );

        return CompletableFuture.completedFuture(fallbackPrompt);
    }

    private CompletableFuture<String> fallbackGenerateDescription(String title, String ingredients, Throwable ex) {
        log.error("Grok 이미지 묘사 생성 실패 (Fallback 실행): {}", ex.getMessage());
        return CompletableFuture.completedFuture(
                "A delicious high-quality photography of " + title + ", michelin star plating."
        );
    }

    private Mono<RecipeCreateRequestDto> parseGrokResponse(String jsonResponse) {
        return Mono.fromCallable(() -> {
            if (jsonResponse == null || jsonResponse.trim().isEmpty()) {
                throw new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED, "Grok API 응답이 비어 있습니다.");
            }

            String cleanedJson = null;

            try {
                Map<String, Object> responseMap = objectMapper.readValue(jsonResponse, new TypeReference<Map<String, Object>>() {
                });
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

    private CompletableFuture<RecipeCreateRequestDto> fallbackGenerate(String prompt, Throwable ex) {
        log.error("Grok fallback 실행: {}", ex.getMessage(), ex);
        return CompletableFuture.failedFuture(
                new CustomException(ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Grok 레시피 생성 실패 (재시도/서킷/타임아웃): " + ex.getMessage(),
                        ex)
        );
    }

    private RecipeAnalysisResponseDto parseAnalysisResponse(String jsonResponse) {
        try {
            Map<String, Object> responseMap = objectMapper.readValue(jsonResponse, new TypeReference<>() {
            });
            List<Map<String, Object>> choices = (List<Map<String, Object>>) responseMap.get("choices");
            Map<String, Object> message = (Map<String, Object>) choices.get(0).get("message");
            String content = message.get("content").toString();

            String cleanedJson = content.replaceAll("(?s)```json\\s*", "").replaceAll("(?s)```\\s*", "").trim();

            return objectMapper.readValue(cleanedJson, RecipeAnalysisResponseDto.class);
        } catch (Exception e) {
            log.error("분석 결과 파싱 실패", e);
            throw new CustomException(ErrorCode.INTERNAL_SERVER_ERROR, "분석 결과 파싱 실패");
        }
    }
}
