package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.chat.prompt.PromptLoader;
import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.metadata.Usage;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.ai.openai.api.OpenAiApi;

import java.util.List;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class AnswerGenerator {

    private final ChatClient proClient;
    private final PromptLoader prompts;

    public AnswerGenerator(@Qualifier("proChatClient") ChatClient proClient,
                           PromptLoader prompts) {
        this.proClient = proClient;
        this.prompts = prompts;
    }

    @Retry(name = "chatbotPro", fallbackMethod = "fallbackGenerate")
    @CircuitBreaker(name = "chatbotPro", fallbackMethod = "fallbackGenerate")
    public GenerationResult generate(String question, String recipeText, List<Message> history) {
        String systemPrompt = prompts.get("chat").replace("{RECIPE}", recipeText);

        ChatClient.ChatClientRequestSpec spec = proClient.prompt().system(systemPrompt);
        if (history != null && !history.isEmpty()) {
            spec = spec.messages(history);
        }
        ChatResponse response = spec.user(question).call().chatResponse();

        String answer = response.getResult().getOutput().getText();

        Usage usage = response.getMetadata().getUsage();
        int input = safeInt(usage != null ? usage.getPromptTokens() : null);
        int output = safeInt(usage != null ? usage.getCompletionTokens() : null);
        int cached = extractCachedTokens(usage);

        return new GenerationResult(answer, input, cached, output);
    }

    private GenerationResult fallbackGenerate(String question, String recipeText, List<Message> history, Throwable e) {
        log.error("Pro answer generation failed", e);
        throw new CustomException(ErrorCode.CHAT_ANSWER_GENERATION_FAILED);
    }

    private int safeInt(Integer v) {
        return v != null ? v : 0;
    }

    private int extractCachedTokens(Usage usage) {
        if (usage == null) return 0;
        Object nativeUsage = usage.getNativeUsage();
        if (nativeUsage instanceof OpenAiApi.Usage openAi) {
            OpenAiApi.Usage.PromptTokensDetails details = openAi.promptTokensDetails();
            if (details != null) {
                return safeInt(details.cachedTokens());
            }
        }
        return 0;
    }
}
