package com.jdc.recipe_service.event;

public class NotificationSavedEvent {
    private final Long notificationId;

    public NotificationSavedEvent(Long notificationId) {
        this.notificationId = notificationId;
    }

    public Long getNotificationId() {
        return notificationId;
    }
}