package com.jdc.recipe_service.domain.entity.chat;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.Generated;
import org.hibernate.generator.EventType;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Entity
@Table(
        name = "chat_daily_usage",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_chat_daily_usage_user_date",
                        columnNames = {"user_id", "usage_date"}
                )
        },
        indexes = {
                @Index(name = "idx_chat_daily_usage_date", columnList = "usage_date")
        }
)
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class ChatDailyUsage {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "user_id", nullable = false)
    private Long userId;

    @Column(name = "usage_date", nullable = false)
    private LocalDate usageDate;

    @Column(name = "call_count", nullable = false)
    @Builder.Default
    private Integer callCount = 0;

    @Generated(event = {EventType.INSERT, EventType.UPDATE})
    @Column(name = "updated_at", insertable = false, updatable = false)
    private LocalDateTime updatedAt;
}
