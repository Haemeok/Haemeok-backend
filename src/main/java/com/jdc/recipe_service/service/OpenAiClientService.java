package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.openai.client.OpenAIClient;
import com.openai.core.JsonValue;
import com.openai.models.ChatModel;
import com.openai.models.chat.completions.ChatCompletion;
import com.openai.models.chat.completions.ChatCompletionCreateParams;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import io.github.resilience4j.timelimiter.annotation.TimeLimiter;
import lombok.RequiredArgsConstructor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
public class OpenAiClientService {

    private static final Logger log = LoggerFactory.getLogger(OpenAiClientService.class);
    private static final boolean LOG_AI = true;

    private final OpenAIClient client;
    private final ObjectMapper objectMapper;

    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @TimeLimiter(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    public CompletableFuture<RecipeCreateRequestDto> generateRecipeJson(String prompt) {
        return CompletableFuture.supplyAsync(() -> {

            if (LOG_AI) log.info("AI Prompt ({} chars):\n{}",
                    prompt != null ? prompt.length() : 0, abbreviate(prompt, 3000));

            var params = ChatCompletionCreateParams.builder()
                    .model(ChatModel.GPT_4_TURBO)
                    .temperature(0.0)
                    .maxCompletionTokens(1800L)
                    .putAdditionalBodyProperty("response_format",
                            JsonValue.from(Map.of("type", "json_object")))
                    .addSystemMessage("너는 한국요리 전문가야. 오직 JSON 객체로만 응답해.")
                    .addUserMessage(prompt)
                    .build();

            ChatCompletion completion;
            try {
                completion = client.chat().completions().create(params);
            } catch (RuntimeException e) {
                log.error("OpenAI 호출 실패: {}", e.getMessage(), e);
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "AI 호출 실패: " + e.getMessage(), e
                );
            }

            if (LOG_AI) {
                String cid = safe(() -> completion.id());
                String model = safe(() -> completion.model());
                var usage = safe(() -> completion.usage().orElse(null));
                log.info("OpenAI completion meta: id={}, model={}, usage={}", cid, model, usage);
            }

            List<ChatCompletion.Choice> choices = completion.choices();
            if (choices.isEmpty()) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "AI 응답이 비어 있습니다."
                );
            }

            String json = choices.get(0).message().content().orElse(null);
            if (json == null) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "AI 응답 내용이 없습니다."
                );
            }

            if (LOG_AI) {
                log.info("AI Raw JSON ({} chars):\n{}", json.length(), abbreviate(json, 4000));
            }

            try {
                return objectMapper.readValue(json, RecipeCreateRequestDto.class);
            } catch (Exception e) {
                log.warn("AI JSON 파싱 실패: {} | raw:\n{}", e.getMessage(), abbreviate(json, 1200));
                throw new CustomException(
                        ErrorCode.INTERNAL_SERVER_ERROR,
                        "AI JSON 파싱 실패: " + e.getMessage(), e
                );
            }
        });
    }

    private CompletableFuture<RecipeCreateRequestDto> fallbackGenerate(String prompt, Throwable ex) {
        if (LOG_AI) log.error("AI 레시피 생성 실패 (fallback): {}", ex.getMessage(), ex);
        return CompletableFuture.failedFuture(
                new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "AI 레시피 생성 실패 (최대 재시도/서킷/타임아웃): " + ex.getMessage(),
                        ex
                )
        );
    }

    private static String abbreviate(String s, int max) {
        if (s == null) return "null";
        if (s.length() <= max) return s;
        return s.substring(0, max) + "...(+" + (s.length() - max) + " chars)";
    }

    private static <T> T safe(ThrowingSupplier<T> s) {
        try { return s.get(); } catch (Throwable t) { return null; }
    }
    @FunctionalInterface
    private interface ThrowingSupplier<T> { T get() throws Exception; }
}
