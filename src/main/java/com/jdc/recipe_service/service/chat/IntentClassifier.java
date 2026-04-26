package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.type.chat.Intent;
import com.jdc.recipe_service.service.chat.prompt.PromptLoader;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.metadata.Usage;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.ai.retry.TransientAiException;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.ResourceAccessException;

@Service
@Slf4j
public class IntentClassifier {

    private final ChatClient miniClient;
    private final PromptLoader prompts;

    public IntentClassifier(@Qualifier("miniChatClient") ChatClient miniClient,
                            PromptLoader prompts) {
        this.miniClient = miniClient;
        this.prompts = prompts;
    }

    @Retryable(
            retryFor = {
                    ResourceAccessException.class,
                    HttpServerErrorException.class,
                    TransientAiException.class
            },
            maxAttempts = 3,
            backoff = @Backoff(delay = 500, multiplier = 2)
    )
    public ClassificationResult classify(String question) {
        String prompt = prompts.get("classifier").replace("{question}", question);

        ChatResponse response = miniClient.prompt()
                .user(prompt)
                .call()
                .chatResponse();

        String label = response.getResult().getOutput().getText();
        Intent intent = Intent.fromString(label);
        if (intent == Intent.UNKNOWN) {
            intent = Intent.IN_SCOPE;
        }

        Usage usage = response.getMetadata().getUsage();
        int input = safeInt(usage != null ? usage.getPromptTokens() : null);
        int output = safeInt(usage != null ? usage.getCompletionTokens() : null);

        return new ClassificationResult(intent, input, output);
    }

    @Recover
    public ClassificationResult recoverClassify(Exception e, String question) {
        log.warn("Mini classification failed after retries, fallback to IN_SCOPE", e);
        return new ClassificationResult(Intent.IN_SCOPE, 0, 0);
    }

    private int safeInt(Integer v) {
        return v != null ? v : 0;
    }
}
