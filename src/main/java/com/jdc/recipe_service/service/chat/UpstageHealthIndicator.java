package com.jdc.recipe_service.service.chat;

import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.openai.OpenAiChatOptions;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.actuate.health.Health;
import org.springframework.stereotype.Component;

// HealthIndicator 인터페이스 미구현 — /actuator/health 자동 등록을 피하고
// 관리자 명시 호출(/api/health/ai)에서만 ping 발생하도록 한다.
@Component
@Slf4j
public class UpstageHealthIndicator {

    private final ChatClient miniClient;

    public UpstageHealthIndicator(@Qualifier("miniChatClient") ChatClient miniClient) {
        this.miniClient = miniClient;
    }

    public Health health() {
        try {
            miniClient.prompt()
                    .user("ping")
                    .options(OpenAiChatOptions.builder()
                            .model("solar-mini")
                            .maxTokens(1)
                            .build())
                    .call()
                    .content();
            return Health.up()
                    .withDetail("provider", "Upstage")
                    .withDetail("model", "solar-mini")
                    .build();
        } catch (Exception e) {
            log.warn("Upstage health check failed", e);
            return Health.down()
                    .withDetail("error", e.getClass().getSimpleName())
                    .withDetail("message", e.getMessage())
                    .build();
        }
    }
}
