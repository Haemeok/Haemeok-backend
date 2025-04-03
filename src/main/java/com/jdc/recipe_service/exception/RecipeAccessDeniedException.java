package com.jdc.recipe_service.exception;

public class RecipeAccessDeniedException extends RuntimeException {
    public RecipeAccessDeniedException(String message) {
        super(message);
    }
}