package com.jdc.recipe_service.event;

import com.jdc.recipe_service.domain.dto.notification.NotificationCreateDto;
import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
import com.jdc.recipe_service.service.AsyncImageService;
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

    @Async
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onAiRecipeCreated(AiRecipeCreatedEvent event) {
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