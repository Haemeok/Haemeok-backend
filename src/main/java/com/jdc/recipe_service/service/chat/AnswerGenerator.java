package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.chat.prompt.PromptLoader;
import lombok.extern.slf4j.Slf4j;
import org.springframework.ai.chat.client.ChatClient;
import org.springframework.ai.chat.messages.Message;
import org.springframework.ai.chat.metadata.Usage;
import org.springframework.ai.chat.model.ChatResponse;
import org.springframework.ai.openai.api.OpenAiApi;
import org.springframework.ai.retry.TransientAiException;

import java.util.List;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.ResourceAccessException;

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

    @Retryable(
            retryFor = {
                    ResourceAccessException.class,
                    HttpServerErrorException.class,
                    TransientAiException.class
            },
            maxAttempts = 3,
            backoff = @Backoff(delay = 500, multiplier = 2)
    )
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

    @Recover
    public GenerationResult recoverGenerate(Exception e, String question, String recipeText, List<Message> history) {
        log.error("Pro answer generation failed after retries", e);
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
