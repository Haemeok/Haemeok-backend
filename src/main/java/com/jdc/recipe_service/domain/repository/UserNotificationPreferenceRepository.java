package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.UserNotificationPreference;
import com.jdc.recipe_service.domain.type.NotificationType;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface UserNotificationPreferenceRepository
        extends JpaRepository<UserNotificationPreference, Long> {

    Optional<UserNotificationPreference>
    findByUserIdAndNotificationType(Long userId, NotificationType type);

    List<UserNotificationPreference>
    findByUserId(Long userId);
}

