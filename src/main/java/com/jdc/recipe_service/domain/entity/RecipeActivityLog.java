package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.ActivityLogType;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "recipe_activity_logs", indexes = {
        @Index(name = "idx_activity_created_at", columnList = "created_at"),
        @Index(name = "idx_activity_user_id", columnList = "user_id")
})
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RecipeActivityLog extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 50)
    private ActivityLogType activityType;

    @Column(nullable = false)
    private Long userId;

    @Column(length = 50)
    private String nickname;
}