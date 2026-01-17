package com.jdc.recipe_service.domain.type;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum ActivityLogType {

    YOUTUBE_EXTRACT("YOUTUBE_EXTRACT"),

    AI_RECIPE_INGREDIENT_FOCUS("AI_RECIPE_INGREDIENT_FOCUS"),
    AI_RECIPE_COST_EFFECTIVE("AI_RECIPE_COST_EFFECTIVE"),
    AI_RECIPE_NUTRITION_BALANCE("AI_RECIPE_NUTRITION_BALANCE"),
    AI_RECIPE_FINE_DINING("AI_RECIPE_FINE_DINING");

    private final String value;

    public static ActivityLogType fromConcept(AiRecipeConcept concept) {
        return ActivityLogType.valueOf("AI_RECIPE_" + concept.name());
    }
}