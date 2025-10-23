package com.jdc.recipe_service.event;

import com.jdc.recipe_service.domain.dto.notification.NotificationCreateDto;
import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.service.AsyncImageService;
import com.jdc.recipe_service.service.AsyncImageServiceV2;
import com.jdc.recipe_service.service.NotificationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

@Component
@RequiredArgsConstructor
@Slf4j
public class AiRecipeEventListener {
    private final AsyncImageService asyncImageService;
    private final NotificationService notificationService;
    private final RecipeIndexingService recipeIndexingService;

    @Async
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onAiRecipeCreated(AiRecipeCreatedEvent event) {

        try {
            recipeIndexingService.indexRecipeSafelyWithRetry(event.getRecipeId());
        } catch (Exception e) {
            log.error("AI 레시피 생성 후 OpenSearch 인덱싱 시도 실패 (리스너 레벨)", e);
        }

        asyncImageService.generateAndUploadAiImageAsync(event.getRecipeId())
                .thenAccept(imageUrl -> {
                    notificationService.createNotification(
                            NotificationCreateDto.builder()
                                    .userId(event.getUserId())
                                    .actorId(null)
                                    .actorNickname(null)
                                    .imageUrl(imageUrl)
                                    .type(NotificationType.AI_RECIPE_DONE)
                                    .relatedType(NotificationRelatedType.RECIPE)
                                    .relatedId(event.getRecipeId())
                                    .relatedUrl("/recipes/" + event.getRecipeId())
                                    .build()
                    );
                })
                .exceptionally(ex -> {
                    log.error("AI 레시피 생성 후 알림 처리 실패", ex);
                    return null;
                });
    }
}