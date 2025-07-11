package com.jdc.recipe_service.event;

import com.jdc.recipe_service.domain.dto.notification.NotificationDto;
import com.jdc.recipe_service.domain.entity.Notification;
import com.jdc.recipe_service.domain.repository.NotificationRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.event.TransactionPhase;
import org.springframework.transaction.event.TransactionalEventListener;

@Service
@RequiredArgsConstructor
public class NotificationEventListener {

    private final NotificationRepository notificationRepo;
    private final SimpMessagingTemplate messagingTemplate;

    @TransactionalEventListener(phase = TransactionPhase.AFTER_COMMIT)
    public void onNotificationSaved(NotificationSavedEvent event) {
        Notification n = notificationRepo.findById(event.getNotificationId())
                .orElseThrow();
        messagingTemplate.convertAndSendToUser(
                String.valueOf(n.getUser().getId()),
                "/queue/notifications",
                NotificationDto.fromEntity(n)
        );
    }
}