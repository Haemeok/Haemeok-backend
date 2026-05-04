package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.type.chat.Intent;
import com.jdc.recipe_service.service.chat.prompt.PromptLoader;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.messages.AssistantMessage;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.messages.UserMessage;
import org.springframework.ai.chat.metadata.Usage;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@Slf4j
public class IntentClassifier {

    private static final int MAX_CONTEXT_CHARS = 800;

    private final ChatClient miniClient;
    private final PromptLoader prompts;

    public IntentClassifier(@Qualifier("miniChatClient") ChatClient miniClient,
                            PromptLoader prompts) {
        this.miniClient = miniClient;
        this.prompts = prompts;
    }

    @Retry(name = "chatbotMini", fallbackMethod = "fallbackClassify")
    @CircuitBreaker(name = "chatbotMini", fallbackMethod = "fallbackClassify")
    public ClassificationResult classify(String question) {
        return classifyInternal(question, List.of());
    }

    @Retry(name = "chatbotMini", fallbackMethod = "fallbackClassify")
    @CircuitBreaker(name = "chatbotMini", fallbackMethod = "fallbackClassify")
    public ClassificationResult classify(String question, List<Message> history) {
        return classifyInternal(question, history);
    }

    private ClassificationResult classifyInternal(String question, List<Message> history) {
        String prompt = prompts.get("classifier")
                .replace("{context}", buildContext(history))
                .replace("{question}", question);

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

    private ClassificationResult fallbackClassify(String question, List<Message> history, Throwable e) {
        log.warn("Mini classification failed, fallback to IN_SCOPE", e);
        return new ClassificationResult(Intent.IN_SCOPE, 0, 0);
    }

    private ClassificationResult fallbackClassify(String question, Throwable e) {
        return fallbackClassify(question, List.of(), e);
    }

    private int safeInt(Integer v) {
        return v != null ? v : 0;
    }

    private String buildContext(List<Message> history) {
        if (history == null || history.isEmpty()) {
            return "없음";
        }

        StringBuilder sb = new StringBuilder();
        for (Message message : history) {
            String role = roleOf(message);
            String text = sanitizeLine(message.getText());
            if (text.isBlank()) {
                continue;
            }
            int remaining = MAX_CONTEXT_CHARS - sb.length() - role.length() - 3;
            if (remaining <= 0) {
                break;
            }
            if (text.length() > remaining) {
                text = text.substring(0, remaining).trim();
            }
            sb.append(role).append(": ").append(text).append('\n');
            if (sb.length() >= MAX_CONTEXT_CHARS) {
                break;
            }
        }
        return sb.length() == 0 ? "없음" : sb.toString().trim();
    }

    private String roleOf(Message message) {
        if (message instanceof UserMessage) {
            return "user";
        }
        if (message instanceof AssistantMessage) {
            return "assistant";
        }
        return "context";
    }

    private String sanitizeLine(String text) {
        if (text == null) {
            return "";
        }
        return text.replaceAll("\\s+", " ").trim();
    }
}
