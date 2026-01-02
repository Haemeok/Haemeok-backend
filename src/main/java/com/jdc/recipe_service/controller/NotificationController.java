package com.jdc.recipe_service.controller;

import com.jdc.recipe_service.config.HashIdConfig.DecodeId;
import com.jdc.recipe_service.domain.dto.notification.NotificationDto;
import com.jdc.recipe_service.security.CustomUserDetails;
import com.jdc.recipe_service.service.NotificationService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
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
    public Page<NotificationDto> getAll(
            @AuthenticationPrincipal CustomUserDetails user,
            @RequestParam(required = false) Boolean read,
            @PageableDefault(size = 20, sort = "createdAt", direction = Sort.Direction.DESC) Pageable pg
    ) {
        Long uid = user.getId();
        if (read == null) {
            return service.getNotifications(uid, pg);
        } else if (read) {
            return service.getReadNotifications(uid, pg);
        } else {
            return service.getUnreadNotifications(uid, pg);
        }
    }

    @DeleteMapping
    public ResponseEntity<Void> deleteAll(@AuthenticationPrincipal CustomUserDetails user) {
        service.deleteAllNotifications(user.getId());
        return ResponseEntity.noContent().build();
    }

    @PostMapping("/read-all")
    public ResponseEntity<Void> markAllRead(@AuthenticationPrincipal CustomUserDetails user) {
        service.markAllAsRead(user.getId());
        return ResponseEntity.ok().build();
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<Void> delete(
            @DecodeId Long id,
            @AuthenticationPrincipal CustomUserDetails user
    ) {
        service.deleteNotification(id, user.getId());
        return ResponseEntity.noContent().build();
    }
    @PatchMapping("/{id}/read")
    public ResponseEntity<Void> markRead(
            @DecodeId Long id,
            @AuthenticationPrincipal CustomUserDetails user
    ) {
        service.markAsRead(id, user.getId());
        return ResponseEntity.ok().build();
    }
    @GetMapping("/unread-count")
    public ResponseEntity<Long> getUnreadCount(@AuthenticationPrincipal CustomUserDetails user) {
        long count = service.getUnreadCount(user.getId());
        return ResponseEntity.ok(count);
    }
}