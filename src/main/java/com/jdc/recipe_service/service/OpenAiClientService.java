package com.jdc.recipe_service.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.openai.client.OpenAIClient;
import com.openai.models.ChatModel;
import com.openai.models.chat.completions.ChatCompletion;
import com.openai.models.chat.completions.ChatCompletionCreateParams;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import io.github.resilience4j.timelimiter.annotation.TimeLimiter;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.CompletableFuture;

//@Service
@RequiredArgsConstructor
public class OpenAiClientService {

    private final OpenAIClient client;
    private final ObjectMapper objectMapper;

    /**
     * Resilience4j로 재시도, 서킷브레이커, 타임리미터 적용.
     * @param prompt AI에게 보낼 프롬프트
     * @return CompletableFuture로 감싼 RecipeCreateRequestDto
     */
    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @TimeLimiter(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    public CompletableFuture<RecipeCreateRequestDto> generateRecipeJson(String prompt) {
        return CompletableFuture.supplyAsync(() -> {
            var params = ChatCompletionCreateParams.builder()
                    .model(ChatModel.GPT_4_TURBO)
                    .temperature(0.0)
                    .maxCompletionTokens(1500L)
                    .addSystemMessage("너는 한국요리 전문가야. 오직 JSON 객체로만 응답해.")
                    .addUserMessage(prompt)
                    .build();

            ChatCompletion completion;
            try {
                completion = client.chat().completions().create(params);
            } catch (RuntimeException e) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "AI 호출 실패: " + e.getMessage(), e
                );
            }

            List<ChatCompletion.Choice> choices = completion.choices();
            if (choices.isEmpty()) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "AI 응답이 비어 있습니다."
                );
            }

            String json = choices.get(0)
                    .message()
                    .content()
                    .orElseThrow(() -> new CustomException(
                            ErrorCode.AI_RECIPE_GENERATION_FAILED,
                            "AI 응답 내용이 없습니다."
                    ));

            try {
                return objectMapper.readValue(json, RecipeCreateRequestDto.class);
            } catch (Exception e) {
                throw new CustomException(
                        ErrorCode.INTERNAL_SERVER_ERROR,
                        "AI JSON 파싱 실패: " + e.getMessage(), e
                );
            }
        });
    }

    private CompletableFuture<RecipeCreateRequestDto> fallbackGenerate(String prompt, Throwable ex) {
        return CompletableFuture.failedFuture(
                new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "AI 레시피 생성 실패 (최대 재시도/서킷/타임아웃): " + ex.getMessage(),
                        ex
                )
        );
    }
}
