package com.jdc.recipe_service.service.ai;

import com.jdc.recipe_service.domain.dto.ai.RecipeAnalysisResponseDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeTrashLog;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RecipeTrashLogRepository;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.util.PromptBuilderV3;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Lazy;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeAnalysisService {

    private final RecipeRepository recipeRepository;
    private final RecipeTrashLogRepository trashLogRepository;
    private final GrokClientService grokClientService;
    private final PromptBuilderV3 promptBuilder;

    @Autowired
    @Lazy
    private RecipeService recipeService;

    @Async
    @Transactional
    public void analyzeRecipeAsync(Long recipeId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElse(null);

        if (recipe == null) return;

        try {
            String prompt = promptBuilder.buildAnalysisPrompt(recipe);

            RecipeAnalysisResponseDto response = grokClientService.analyzeRecipe(prompt).join();

            if (response.isAbusive()) {
                handleAbusiveRecipe(recipe);
            } else {
                handleNormalRecipe(recipe, response);
            }

        } catch (Exception e) {
            log.error("레시피 분석 중 오류 발생 ID: " + recipeId, e);
            recipe.updateAiAnalysisStatus("FAILED");
            recipeRepository.save(recipe);
        }
    }

    private void handleAbusiveRecipe(Recipe recipe) {
        log.warn("유해 레시피 감지됨 (삭제 예정): {}", recipe.getTitle());

        String ingredientsSnapshot = recipe.getIngredients() != null ? recipe.getIngredients().toString() : "[]";
        String stepsSnapshot = recipe.getSteps() != null ? recipe.getSteps().toString() : "[]";

        RecipeTrashLog trashLog = RecipeTrashLog.builder()
                .originalRecipeId(recipe.getId())
                .userId(recipe.getUser().getId())
                .title(recipe.getTitle())
                .ingredientsSnapshot(ingredientsSnapshot)
                .instructionSnapshot(stepsSnapshot)
                .detectedReason("ABUSIVE_CONTENT") //
                .build();

        trashLogRepository.save(trashLog);

        recipeService.deleteRecipe(recipe.getId(), recipe.getUser().getId());
    }

    private void handleNormalRecipe(Recipe recipe, RecipeAnalysisResponseDto response) {
        recipe.updateAiInfo(response.getCookingTips(), response.getMarketPrice());
        recipe.updateAiAnalysisStatus("COMPLETED");

        recipeRepository.save(recipe);
    }
}