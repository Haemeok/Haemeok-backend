package com.jdc.recipe_service.domain.type.recipe;

import lombok.Getter;

@Getter
public enum RecipeSourceType {
    USER,
    AI,
    YOUTUBE,
    REELS;
}
