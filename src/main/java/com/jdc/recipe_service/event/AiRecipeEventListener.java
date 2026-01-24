package com.jdc.recipe_service.event;

import com.jdc.recipe_service.domain.dto.notification.NotificationCreateDto;
import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
import com.jdc.recipe_service.opensearch.service.RecipeIndexingService;
import com.jdc.recipe_service.service.image.AsyncImageService;
import com.jdc.recipe_service.service.NotificationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.hashids.Hashids;
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
    private final Hashids hashids;

    @Async
    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onAiRecipeCreated(AiRecipeCreatedEvent event) {
        try {
            String imageUrl = asyncImageService.generateAndUploadAiImage(event.getRecipeId());

            String encodedId = hashids.encode(event.getRecipeId());
            notificationService.createNotification(
                    NotificationCreateDto.builder()
                            .userId(event.getUserId())
                            .actorId(null)
                            .actorNickname(null)
                            .imageUrl(imageUrl)
                            .type(NotificationType.AI_RECIPE_DONE)
                            .relatedType(NotificationRelatedType.RECIPE)
                            .relatedId(event.getRecipeId())
                            .relatedUrl("/recipes/" + encodedId)
                            .build()
            );

        } catch (Exception ex) {
            log.error("AI 레시피 이미지 생성 실패로 인해 알림 발송 중단", ex);
        }
    }
}