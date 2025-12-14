package com.jdc.recipe_service.service.ai;

import com.jdc.recipe_service.domain.dto.ai.RecipeAnalysisResponseDto;
import com.jdc.recipe_service.domain.dto.notification.NotificationCreateDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeTrashLog;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RecipeTrashLogRepository;
import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
import com.jdc.recipe_service.service.NotificationService;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.util.prompt.RecipeAnalysisPromptBuilder;
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
    private final RecipeAnalysisPromptBuilder promptBuilder;
    private final NotificationService notificationService;

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

            boolean isReallyAbusive = response.isAbusive() ||
                    (response.getAbuseType() != null && !"SAFE".equalsIgnoreCase(response.getAbuseType()));

            if (isReallyAbusive) {
                try {
                    handleAbusiveRecipe(recipe, response);
                } catch (Exception e) {
                    log.error("유해 레시피 삭제 과정 중 에러 발생 (OpenSearch 등): {}", e.getMessage());
                }
            } else {
                handleNormalRecipe(recipe, response);
            }

        } catch (Exception e) {
            log.error("레시피 분석 중 오류 발생 ID: " + recipeId, e);
            recipe.updateAiAnalysisStatus("FAILED");
            recipeRepository.save(recipe);
        }
    }

    private void handleAbusiveRecipe(Recipe recipe, RecipeAnalysisResponseDto response) {
        log.warn("유해 레시피 감지됨 (삭제 예정): {}", recipe.getTitle());

        String ingredientsSnapshot = recipe.getIngredients() != null ? recipe.getIngredients().toString() : "[]";
        String stepsSnapshot = recipe.getSteps() != null ? recipe.getSteps().toString() : "[]";
        String reason = (response.getAbuseType() != null) ? response.getAbuseType() : "ABUSIVE_CONTENT";

        RecipeTrashLog trashLog = RecipeTrashLog.builder()
                .originalRecipeId(recipe.getId())
                .userId(recipe.getUser().getId())
                .title(recipe.getTitle())
                .ingredientsSnapshot(ingredientsSnapshot)
                .instructionSnapshot(stepsSnapshot)
                .detectedReason(reason)
                .build();

        trashLogRepository.save(trashLog);

        recipeService.deleteRecipe(recipe.getId(), recipe.getUser().getId());

        try {
            NotificationCreateDto notificationDto = NotificationCreateDto.builder()
                    .userId(recipe.getUser().getId())
                    .actorId(null)
                    .actorNickname("System")
                    .type(NotificationType.RECIPE_POLICY_VIOLATION)
                    .relatedType(NotificationRelatedType.RECIPE)
                    .relatedId(recipe.getId())
                    .relatedUrl(null)
                    .message(reason)
                    .imageUrl(null)
                    .build();

            notificationService.createNotification(notificationDto);
            log.info("유해 레시피 삭제 알림 발송 완료. User: {}, Reason: {}", recipe.getUser().getId(), reason);

        } catch (Exception e) {
            log.error("알림 발송 실패 (삭제는 정상 진행됨): {}", e.getMessage());
        }
    }

    private void handleNormalRecipe(Recipe recipe, RecipeAnalysisResponseDto response) {
        recipeRepository.updateAiAnalysisResult(
                recipe.getId(),
                response.getCookingTips(),
                response.getMarketPrice(),
                "COMPLETED"
        );
    }
}