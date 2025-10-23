package com.jdc.recipe_service.event;

public class UserRecipeCreatedEvent {
    private final Long recipeId;

    public UserRecipeCreatedEvent(Long recipeId) {
        this.recipeId = recipeId;
    }

    public Long getRecipeId() {
        return recipeId;
    }
}