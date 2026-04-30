package com.jdc.recipe_service.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.client.WebClient;
import jakarta.annotation.PostConstruct;

/**
 * Grok(xAI) WebClient 설정.
 *
 * <p>운영자 가이드 — 환경변수는 OpenAI와 명확히 분리된다:
 * <ul>
 *   <li><b>GROK_API_KEY</b> (필수): xAI 발급 키. application-local.yml / application-prod.yml의
 *       {@code grok.api-key}로 매핑.</li>
 *   <li><b>OPENAI_API_KEY</b>: 진짜 OpenAI(이미지 생성용) — 별도. application.yml의
 *       {@code app.openai.api-key}로 매핑되며 이 config와는 무관.</li>
 * </ul>
 *
 * <p>이전 fallback chain({@code grok.api-key:${openai.api-key:...}})은 prefix 혼동을 유발해 제거.
 * GitHub Actions secrets에 GROK_API_KEY가 별도 등록되어 있어 fallback 가치 없음.
 */
@Slf4j
@Configuration
public class GrokWebClientConfig {

    @Value("${grok.api-key}")
    private String apiKey;

    @Value("${grok.base-url:https://api.x.ai/v1}")
    private String baseUrl;

    @Bean("grokWebClient")
    public WebClient grokWebClient() {
        return WebClient.builder()
                .baseUrl(baseUrl)
                .defaultHeader("Authorization", "Bearer " + apiKey)
                .defaultHeader("Content-Type", "application/json")
                .build();
    }

    @PostConstruct
    public void validateApiKey() {
        if (apiKey == null || apiKey.isBlank()) {
            throw new IllegalArgumentException(
                    "API key for Grok must be set. (env: GROK_API_KEY, yml: grok.api-key)");
        }
        // 시크릿 자체는 출력 안 함 — 길이만 부팅 로그에 표시해 어느 키가 적용됐는지 운영 검증 가능.
        log.info("[Grok] WebClient ready. baseUrl={}, apiKey length={} chars", baseUrl, apiKey.length());
    }
}
