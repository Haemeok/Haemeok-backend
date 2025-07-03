package com.jdc.recipe_service.domain.dto.notification;

import com.jdc.recipe_service.domain.entity.Notification;
import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
import lombok.*;

import java.time.LocalDateTime;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NotificationDto {
    private Long id;
    private Long userId;
    private Long actorId;
    private NotificationType type;
    private String content;
    private NotificationRelatedType relatedType;
    private Long relatedId;
    private String relatedUrl;
    private boolean isRead;
    private LocalDateTime createdAt;

    public static NotificationDto fromEntity(Notification n) {
        return NotificationDto.builder()
                .id(n.getId())
                .userId(n.getUser().getId())
                .actorId(n.getActor() != null ? n.getActor().getId() : null)
                .type(n.getType())
                .content(n.getContent())
                .relatedType(n.getRelatedType())
                .relatedId(n.getRelatedId())
                .relatedUrl(n.getRelatedUrl())
                .isRead(n.getIsRead())
                .createdAt(n.getCreatedAt())
                .build();
    }
}

