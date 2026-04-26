package com.jdc.recipe_service.config.chat;

import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.openai.OpenAiChatModel;
import org.springframework.ai.openai.OpenAiChatOptions;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * 챗봇용 Upstage Solar 클라이언트. 단일 OpenAiChatModel을 공유하며 Mini/Pro 옵션만 분리한다.
 */
@Configuration
public class AiClientConfig {

    @Bean
    public ChatClient proChatClient(OpenAiChatModel chatModel) {
        return ChatClient.builder(chatModel)
                .defaultOptions(OpenAiChatOptions.builder()
                        .model("solar-pro3")
                        .temperature(0.5)
                        .maxTokens(600)
                        .build())
                .build();
    }

    @Bean
    public ChatClient miniChatClient(OpenAiChatModel chatModel) {
        return ChatClient.builder(chatModel)
                .defaultOptions(OpenAiChatOptions.builder()
                        .model("solar-mini")
                        .temperature(0.0)
                        .maxTokens(20)
                        .build())
                .build();
    }
}
