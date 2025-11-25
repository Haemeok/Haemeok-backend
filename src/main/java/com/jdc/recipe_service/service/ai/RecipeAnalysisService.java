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
import com.jdc.recipe_service.util.PromptBuilderV3;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.support.TransactionTemplate; // ★ 추가됨

@Service
@RequiredArgsConstructor
@Slf4j
public class RecipeAnalysisService {

    private final RecipeRepository recipeRepository;
    private final RecipeTrashLogRepository trashLogRepository;
    private final GrokClientService grokClientService;
    private final PromptBuilderV3 promptBuilder;
    private final NotificationService notificationService;
    private final TransactionTemplate transactionTemplate;

    @Autowired
    @Lazy
    private RecipeService recipeService;

    @Async
    public void analyzeRecipeAsync(Long recipeId) {
        Recipe recipe = recipeRepository.findById(recipeId).orElse(null);
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
                    log.error("유해 레시피 삭제 과정 중 에러 발생: {}", e.getMessage());
                }
            } else {
                transactionTemplate.execute(status -> {
                    handleNormalRecipe(recipe, response);
                    return null;
                });
            }

        } catch (Exception e) {
            log.error("레시피 분석 중 오류 발생 ID: " + recipeId, e);

            if (recipeRepository.existsById(recipeId)) {
                transactionTemplate.execute(status -> {
                    updateAnalysisStatusToFailed(recipeId);
                    return null;
                });
            }
        }
    }

    private void handleAbusiveRecipe(Recipe recipe, RecipeAnalysisResponseDto response) {
        log.warn("유해 레시피 감지됨 (삭제 예정): {}", recipe.getTitle());

        String ingredientsSnapshot = recipe.getIngredients() != null ? recipe.getIngredients().toString() : "[]";
        String stepsSnapshot = recipe.getSteps() != null ? recipe.getSteps().toString() : "[]";
        String reason = (response.getAbuseType() != null) ? response.getAbuseType() : "ABUSIVE_CONTENT";

        transactionTemplate.execute(status -> {
            RecipeTrashLog trashLog = RecipeTrashLog.builder()
                    .originalRecipeId(recipe.getId())
                    .userId(recipe.getUser().getId())
                    .title(recipe.getTitle())
                    .ingredientsSnapshot(ingredientsSnapshot)
                    .instructionSnapshot(stepsSnapshot)
                    .detectedReason(reason)
                    .build();
            trashLogRepository.save(trashLog);
            return null;
        });

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
        recipeRepository.findById(recipe.getId()).ifPresent(r -> {
            recipeRepository.updateAiAnalysisResult(
                    r.getId(),
                    response.getCookingTips(),
                    response.getMarketPrice(),
                    "COMPLETED"
            );
        });
    }

    private void updateAnalysisStatusToFailed(Long recipeId) {
        recipeRepository.findById(recipeId).ifPresent(r -> {
            r.updateAiAnalysisStatus("FAILED");
        });
    }
}