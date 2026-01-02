package com.jdc.recipe_service.domain.dto.notification;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class NotificationCreateDto {
    @JsonDeserialize(using = HashIdConfig.HashIdDeserializer.class)
    private Long userId;
    @JsonDeserialize(using = HashIdConfig.HashIdDeserializer.class)
    private Long actorId;
    private String actorNickname;
    private String imageUrl;
    private NotificationType type;
    private NotificationRelatedType relatedType;
    @JsonDeserialize(using = HashIdConfig.HashIdDeserializer.class)
    private Long relatedId;
    private String relatedUrl;
    private String message;
}
