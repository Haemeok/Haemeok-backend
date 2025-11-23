package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.NotificationRelatedType;
import com.jdc.recipe_service.domain.type.NotificationType;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.OnDelete;
import org.hibernate.annotations.OnDeleteAction;

import java.time.LocalDateTime;

@Entity
@Table(
        name = "notifications",
        indexes = @Index(name = "idx_notifications_query", columnList = "user_id,is_read,created_at")
)
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class Notification extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false, foreignKey = @ForeignKey(name = "fk_notification_user"))
    @OnDelete(action = OnDeleteAction.CASCADE)
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "actor_id", foreignKey = @ForeignKey(name = "fk_notification_actor"))
    @OnDelete(action = OnDeleteAction.SET_NULL)
    private User actor;

    @Enumerated(EnumType.STRING)
    @Column(name = "type", length = 32, nullable = false)
    private NotificationType type;

    @Column(name = "actor_nickname", nullable = true)
    private String actorNickname;

    @Column(name = "image_url", nullable = true)
    private String imageUrl;

    @Enumerated(EnumType.STRING)
    @Column(name = "related_type", length = 32)
    private NotificationRelatedType relatedType;

    @Column(name = "related_id")
    private Long relatedId;

    @Column(name = "related_url", length = 255)
    private String relatedUrl;

    @Column(name = "message", length = 255)
    private String message;

    @Column(name = "is_read", nullable = false)
    @Builder.Default
    private Boolean isRead = false;

    private LocalDateTime readAt;
}
