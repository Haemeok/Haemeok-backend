package com.jdc.recipe_service.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.client.WebClient;
import jakarta.annotation.PostConstruct;

@Configuration
public class GrokWebClientConfig {

    @Value("${openai.api-key}")
    private String apiKey;

    @Value("${openai.base-url}")
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
        if (apiKey == null || apiKey.isEmpty()) {
            throw new IllegalArgumentException("API key for Grok/OpenAI must be set.");
        }
    }
}