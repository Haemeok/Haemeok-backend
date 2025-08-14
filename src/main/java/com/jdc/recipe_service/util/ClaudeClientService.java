package com.jdc.recipe_service.util;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jdc.recipe_service.domain.dto.recipe.RecipeCreateRequestDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import io.github.resilience4j.timelimiter.annotation.TimeLimiter;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
@RequiredArgsConstructor
public class ClaudeClientService {

    private final RestTemplate restTemplate;
    private final ObjectMapper objectMapper;

    @Value("${anthropic.api-key}")
    private String apiKey;

    private static final String API_URL = "https://api.anthropic.com/v1/messages";

    @Retry(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @CircuitBreaker(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    @TimeLimiter(name = "aiGenerate", fallbackMethod = "fallbackGenerate")
    public CompletableFuture<RecipeCreateRequestDto> generateRecipeJson(String prompt) {
        return CompletableFuture.supplyAsync(() -> {
            RequestBody body = new RequestBody(
                    "claude-3-sonnet-20240229",
                    List.of(new Message("user", prompt)),
                    1800,
                    0.0,
                    "너는 한국요리 전문가야. 오직 JSON 객체로만 응답해. 다른 부가적인 설명은 절대 추가하지 마."
            );

            // 2) 헤더
            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.APPLICATION_JSON);
            headers.set("x-api-key", apiKey);
            headers.set("anthropic-version", "2023-06-01"); // 필수

            HttpEntity<RequestBody> entity = new HttpEntity<>(body, headers);

            // 3) 호출
            ResponseEntity<ResponseBody> res;
            try {
                res = restTemplate.exchange(API_URL, HttpMethod.POST, entity, ResponseBody.class);
            } catch (RuntimeException e) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Claude API 호출 실패: " + e.getMessage(), e
                );
            }

            ResponseBody response = res.getBody();
            if (response == null || response.content == null || response.content.isEmpty()) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Claude API 응답이 비어 있습니다."
                );
            }

            StringBuilder sb = new StringBuilder();
            for (ContentBlock block : response.content) {
                if ("text".equals(block.type) && block.text != null) {
                    sb.append(block.text);
                    if (!block.text.endsWith("\n")) sb.append('\n');
                }
            }
            String raw = sb.toString().trim();
            if (raw.isEmpty()) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "Claude API 응답에서 텍스트 블록을 찾지 못했습니다."
                );
            }

            String json = extractJson(raw);
            if (json.isEmpty()) {
                throw new CustomException(
                        ErrorCode.AI_RECIPE_GENERATION_FAILED,
                        "AI 응답에서 유효한 JSON 객체를 찾을 수 없습니다."
                );
            }

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

    /** 응답 문자열에서 최상위 JSON 객체만 추출 */
    private String extractJson(String response) {
        Pattern pattern = Pattern.compile("\\{.*\\}", Pattern.DOTALL);
        Matcher matcher = pattern.matcher(response);
        if (matcher.find()) return matcher.group();
        return "";
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

    @Data @AllArgsConstructor @NoArgsConstructor
    private static class RequestBody {
        public String model;

        public List<Message> messages;

        @JsonProperty("max_tokens")
        public Integer maxTokens;

        public Double temperature;

        public String system;
    }

    @Data @AllArgsConstructor @NoArgsConstructor
    private static class Message {
        public String role;
        public String content;
    }

    @Data @JsonIgnoreProperties(ignoreUnknown = true)
    private static class ResponseBody {
        public String id;
        public String type;
        public List<ContentBlock> content;
    }

    @Data @JsonIgnoreProperties(ignoreUnknown = true)
    private static class ContentBlock {
        public String type;
        public String text;
    }
}
