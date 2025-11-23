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
    private String actorNickname;
    private String imageUrl;
    private NotificationType type;
    private NotificationRelatedType relatedType;
    private Long relatedId;
    private String relatedUrl;
    private String message;
    private boolean isRead;
    private LocalDateTime createdAt;

    public static NotificationDto fromEntity(Notification n) {
        return NotificationDto.builder()
                .id(n.getId())
                .userId(n.getUser().getId())
                .actorId(n.getActor() != null ? n.getActor().getId() : null)
                .actorNickname(n.getActorNickname())
                .imageUrl(n.getImageUrl())
                .type(n.getType())
                .relatedType(n.getRelatedType())
                .relatedId(n.getRelatedId())
                .relatedUrl(n.getRelatedUrl())
                .message(n.getMessage())
                .isRead(n.getIsRead())
                .createdAt(n.getCreatedAt())
                .build();
    }
}

