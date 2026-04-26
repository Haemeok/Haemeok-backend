package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

@Component
@RequiredArgsConstructor
public class ChatRecipeLoader {

    private final RecipeRepository recipeRepository;

    @Transactional(readOnly = true)
    public String loadAsPromptText(Long userId, Long recipeId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        if (Boolean.TRUE.equals(recipe.getIsPrivate())) {
            if (userId == null || !recipe.getUser().getId().equals(userId)) {
                throw new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
            }
        }

        return serialize(recipe);
    }

    private String serialize(Recipe recipe) {
        StringBuilder sb = new StringBuilder();
        sb.append("제목: ").append(safe(recipe.getTitle())).append('\n');
        if (recipe.getDescription() != null) {
            sb.append("설명: ").append(recipe.getDescription()).append('\n');
        }
        if (recipe.getDishType() != null) {
            sb.append("종류: ").append(recipe.getDishType().name()).append('\n');
        }
        if (recipe.getCookingTime() != null) {
            sb.append("조리시간: ").append(recipe.getCookingTime()).append("분\n");
        }
        if (recipe.getServings() != null) {
            sb.append("인분: ").append(recipe.getServings()).append('\n');
        }
        appendIngredients(sb, recipe);
        appendSteps(sb, recipe);
        if (recipe.getCookingTips() != null && !recipe.getCookingTips().isBlank()) {
            sb.append("팁: ").append(recipe.getCookingTips()).append('\n');
        }
        return sb.toString();
    }

    private void appendIngredients(StringBuilder sb, Recipe recipe) {
        if (recipe.getIngredients() == null || recipe.getIngredients().isEmpty()) return;
        sb.append("재료:\n");
        for (RecipeIngredient ing : recipe.getIngredients()) {
            String name = ing.getCustomName() != null
                    ? ing.getCustomName()
                    : (ing.getIngredient() != null ? ing.getIngredient().getName() : "재료");
            String unit = ing.getCustomUnit() != null
                    ? ing.getCustomUnit()
                    : (ing.getUnit() != null ? ing.getUnit() : "");
            sb.append("- ").append(name)
                    .append(' ').append(safe(ing.getQuantity()))
                    .append(' ').append(unit)
                    .append('\n');
        }
    }

    private void appendSteps(StringBuilder sb, Recipe recipe) {
        if (recipe.getSteps() == null || recipe.getSteps().isEmpty()) return;
        sb.append("조리법:\n");
        int i = 1;
        for (RecipeStep step : recipe.getSteps()) {
            sb.append(i++).append(". ").append(safe(step.getInstruction())).append('\n');
        }
    }

    private String safe(String s) {
        return s != null ? s : "";
    }
}
