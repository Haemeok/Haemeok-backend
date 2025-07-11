package com.jdc.recipe_service.event;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter
public class AiRecipeCreatedEvent {
    private final Long recipeId;
    private final Long userId;
}
