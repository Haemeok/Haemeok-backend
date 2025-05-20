package com.jdc.recipe_service.util;

import com.jdc.recipe_service.domain.dto.recipe.AiRecipeRequestDto;

public class PromptBuilder {
    public static String buildPrompt(AiRecipeRequestDto request) {
        return String.format("""
                        다음 조건을 만족하는 레시피를 JSON으로 생성해줘:
                        
                        - 요리 유형: %s
                        - 조리 시간: %d분
                        - 재료: %s
                        - 태그: %s
                        
                        출력은 반드시 JSON만, 설명이나 코드블럭 없이.
                        """,
                request.getDishType(),
                request.getCookingTime(),
                String.join(", ", request.getIngredients()),
                String.join(", ", request.getTagNames()));
    }
}