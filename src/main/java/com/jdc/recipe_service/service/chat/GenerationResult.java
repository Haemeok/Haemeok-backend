package com.jdc.recipe_service.service.chat;

public record GenerationResult(
        String answer,
        int inputTokens,
        int cachedTokens,
        int outputTokens
) {}
