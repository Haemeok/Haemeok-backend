package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.notification.UserNotificationPreferenceDto;
import com.jdc.recipe_service.domain.type.NotificationType;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.NotificationPreferenceService;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/notification-preferences")
@RequiredArgsConstructor
public class NotificationPreferenceController {
    private final NotificationPreferenceService prefService;

    @GetMapping
    public List<UserNotificationPreferenceDto> getPreferences(
            @AuthenticationPrincipal CustomUserDetails user) {
        return prefService.findByUserId(user.getId());
    }

    @PatchMapping("/{type}")
    public ResponseEntity<UserNotificationPreferenceDto> update(
            @PathVariable NotificationType type,
            @RequestBody PreferenceDto dto,
            @AuthenticationPrincipal CustomUserDetails user
    ) {
        var pref = prefService.updatePreference(
                user.getId(), type, dto.isEnabled());
        return ResponseEntity.ok(UserNotificationPreferenceDto.of(pref));
    }

    @Getter
    @Setter
    public static class PreferenceDto {
        private boolean enabled;
    }
}
