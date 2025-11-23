package com.jdc.recipe_service.domain.dto.notification;

import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NotificationCreateDto {
    private Long userId;
    private Long actorId;
    private String actorNickname;
    private String imageUrl;
    private NotificationType type;
    private NotificationRelatedType relatedType;
    private Long relatedId;
    private String relatedUrl;
    private String message;
}
