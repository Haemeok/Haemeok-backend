package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.notification.UserNotificationPreferenceDto;
import com.jdc.recipe_service.domain.entity.UserNotificationPreference;
import com.jdc.recipe_service.domain.repository.UserNotificationPreferenceRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.domain.type.NotificationType;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class NotificationPreferenceService {
    private final UserNotificationPreferenceRepository prefRepo;
    private final UserRepository userRepository;

    @Transactional
    public UserNotificationPreference updatePreference(Long userId, NotificationType type, boolean enabled) {
        return prefRepo.findByUserIdAndNotificationType(userId, type)
                .map(pref -> { pref.setEnabled(enabled); return pref; })
                .orElseGet(() -> prefRepo.save(
                        UserNotificationPreference.builder()
                                .user(userRepository.getReferenceById(userId))
                                .notificationType(type)
                                .enabled(enabled)
                                .build()
                ));
    }

    @Transactional(readOnly = true)
    public List<UserNotificationPreferenceDto> findByUserId(Long userId) {
        return prefRepo.findByUserId(userId).stream()
                .map(UserNotificationPreferenceDto::of)
                .collect(Collectors.toList());
    }
}