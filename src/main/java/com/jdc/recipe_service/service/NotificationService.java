package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.notification.NotificationCreateDto;
import com.jdc.recipe_service.domain.dto.notification.NotificationDto;
import com.jdc.recipe_service.domain.entity.Notification;
import com.jdc.recipe_service.domain.entity.UserNotificationPreference;
import com.jdc.recipe_service.domain.repository.NotificationRepository;
import com.jdc.recipe_service.domain.repository.UserNotificationPreferenceRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.NotificationType;
import lombok.RequiredArgsConstructor;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class NotificationService {
    private final NotificationRepository notificationRepo;
    private final UserNotificationPreferenceRepository prefRepo;
    private final UserRepository userRepository;
    private final SimpMessagingTemplate messagingTemplate;

    @Transactional
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
                .type(type)
                .content(dto.getContent())
                .relatedType(dto.getRelatedType())
                .relatedId(dto.getRelatedId())
                .relatedUrl(dto.getRelatedUrl())
                .build();
        notificationRepo.save(n);

        messagingTemplate.convertAndSendToUser(
                String.valueOf(userProxy.getId()),
                "/queue/notifications",
                NotificationDto.fromEntity(n)
        );
    }

    @Transactional(readOnly = true)
    public List<NotificationDto> getNotifications(Long userId) {
        return notificationRepo.findByUserIdOrderByCreatedAtDesc(userId)
                .stream()
                .map(NotificationDto::fromEntity)
                .collect(Collectors.toList());
    }
    @Transactional(readOnly = true)
    public List<NotificationDto> getUnreadNotifications(Long userId) {
        return notificationRepo
                .findByUserIdAndIsReadFalseOrderByCreatedAtDesc(userId)
                .stream()
                .map(NotificationDto::fromEntity)
                .toList();
    }

    @Transactional(readOnly = true)
    public List<NotificationDto> getReadNotifications(Long userId) {
        return notificationRepo
                .findByUserIdAndIsReadTrueOrderByCreatedAtDesc(userId)
                .stream()
                .map(NotificationDto::fromEntity)
                .toList();
    }

    @Transactional
    public void markAllAsRead(Long userId) {
        var unread = notificationRepo.findByUserIdAndIsReadFalse(userId);
        unread.forEach(n -> n.setIsRead(true));
        notificationRepo.saveAll(unread);
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
}