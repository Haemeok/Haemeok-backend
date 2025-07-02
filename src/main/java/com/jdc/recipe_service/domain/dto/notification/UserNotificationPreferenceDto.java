package com.jdc.recipe_service.domain.dto.notification;

import com.jdc.recipe_service.domain.entity.UserNotificationPreference;
import com.jdc.recipe_service.domain.type.NotificationType;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserNotificationPreferenceDto {
    private NotificationType notificationType;
    private boolean enabled;

    public static UserNotificationPreferenceDto of(UserNotificationPreference pref) {
        return UserNotificationPreferenceDto.builder()
                .notificationType(pref.getNotificationType())
                .enabled(pref.getEnabled())
                .build();
    }
}
