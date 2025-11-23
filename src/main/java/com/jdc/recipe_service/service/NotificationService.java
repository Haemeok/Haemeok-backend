package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.notification.NotificationCreateDto;
import com.jdc.recipe_service.domain.dto.notification.NotificationDto;
import com.jdc.recipe_service.domain.entity.Notification;
import com.jdc.recipe_service.domain.entity.UserNotificationPreference;
import com.jdc.recipe_service.domain.repository.NotificationRepository;
import com.jdc.recipe_service.domain.repository.UserNotificationPreferenceRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.NotificationType;
import com.jdc.recipe_service.event.NotificationSavedEvent;
import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class NotificationService {
    private final NotificationRepository notificationRepo;
    private final UserNotificationPreferenceRepository prefRepo;
    private final UserRepository userRepository;
    private final ApplicationEventPublisher eventPublisher;

    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void createNotification(NotificationCreateDto dto) {
        NotificationType type = dto.getType();

        UserNotificationPreference pref = prefRepo
                .findByUserIdAndNotificationType(dto.getUserId(), type)
                .orElseGet(() -> prefRepo.save(
                        UserNotificationPreference.builder()
                                .user(userRepository.getReferenceById(dto.getUserId()))
                                .notificationType(type)
                                .enabled(true)
                                .build()
                ));
        if (!pref.getEnabled()) return;

        var userProxy = userRepository.getReferenceById(dto.getUserId());
        var actorProxy = dto.getActorId() != null
                ? userRepository.getReferenceById(dto.getActorId())
                : null;

        Notification n = Notification.builder()
                .user(userProxy)
                .actor(actorProxy)
                .actorNickname(dto.getActorNickname())
                .imageUrl(dto.getImageUrl())
                .type(type)
                .relatedType(dto.getRelatedType())
                .relatedId(dto.getRelatedId())
                .relatedUrl(dto.getRelatedUrl())
                .message(dto.getMessage())
                .build();
        notificationRepo.save(n);

        eventPublisher.publishEvent(new NotificationSavedEvent(n.getId()));
    }

    @Transactional(readOnly = true)
    public Page<NotificationDto> getNotifications(Long userId, Pageable pg) {
        return notificationRepo
                .findByUserIdOrderByCreatedAtDesc(userId, pg)
                .map(NotificationDto::fromEntity);
    }
    @Transactional(readOnly = true)
    public Page<NotificationDto> getUnreadNotifications(Long userId, Pageable pg) {
        return notificationRepo
                .findByUserIdAndIsReadFalseOrderByCreatedAtDesc(userId, pg)
                .map(NotificationDto::fromEntity);
    }

    @Transactional(readOnly = true)
    public Page<NotificationDto> getReadNotifications(Long userId, Pageable pg) {
        return notificationRepo
                .findByUserIdAndIsReadTrueOrderByCreatedAtDesc(userId, pg)
                .map(NotificationDto::fromEntity);
    }

    @Transactional
    public void markAllAsRead(Long userId) {
        var unread = notificationRepo.findByUserIdAndIsReadFalse(userId);
        LocalDateTime now = LocalDateTime.now();
        unread.forEach(n -> {
            n.setIsRead(true);
            n.setReadAt(now);
        });
        notificationRepo.saveAll(unread);
    }
    @Transactional
    public void markAsRead(Long notificationId, Long userId) {
        Notification n = notificationRepo.findById(notificationId)
                .orElseThrow(() -> new RuntimeException("알림이 존재하지 않습니다."));
        if (!n.getUser().getId().equals(userId)) {
            throw new RuntimeException("권한이 없습니다.");
        }
        n.setIsRead(true);
        n.setReadAt(LocalDateTime.now());
        notificationRepo.save(n);
    }
    @Transactional
    public void deleteAllNotifications(Long userId) {
        notificationRepo.deleteByUserId(userId);
    }
    @Transactional
    public void deleteNotification(Long id, Long userId) {
        Notification n = notificationRepo.findById(id)
                .orElseThrow(() -> new RuntimeException("알림이 존재하지 않습니다."));
        if (!n.getUser().getId().equals(userId)) {
            throw new RuntimeException("권한이 없습니다.");
        }
        notificationRepo.delete(n);
    }
    @Transactional(readOnly = true)
    public long getUnreadCount(Long userId) {
        return notificationRepo.countByUserIdAndIsReadFalse(userId);
    }
}