package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.type.chat.Intent;

public record ClassificationResult(
        Intent intent,
        int inputTokens,
        int outputTokens
) {}
