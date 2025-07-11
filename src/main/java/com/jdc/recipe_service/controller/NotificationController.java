package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.domain.dto.notification.NotificationDto;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.NotificationService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.annotation.AuthenticationPrincipal;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/notifications")
@RequiredArgsConstructor
public class NotificationController {
    private final NotificationService service;

    @GetMapping
    public List<NotificationDto> getAll(
            @AuthenticationPrincipal CustomUserDetails user,
            @RequestParam(required = false) Boolean read
    ) {
        Long uid = user.getId();
        if (read == null) {
            return service.getNotifications(uid);
        } else if (read) {
            return service.getReadNotifications(uid);
        } else {
            return service.getUnreadNotifications(uid);
        }
    }

    @PostMapping("/read-all")
    public ResponseEntity<Void> markAllRead(@AuthenticationPrincipal CustomUserDetails user) {
        service.markAllAsRead(user.getId());
        return ResponseEntity.ok().build();
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(
            @PathVariable Long id,
            @AuthenticationPrincipal CustomUserDetails user
    ) {
        service.deleteNotification(id, user.getId());
        return ResponseEntity.noContent().build();
    }
}