package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Entity
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@EntityListeners(AuditingEntityListener.class)
@Table(
        name = "user_daily_access",
        indexes = {
                @Index(name = "idx_access_date", columnList = "access_date")
        },
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_user_date",
                        columnNames = {"user_id", "access_date"}
                )
        }
)
public class UserDailyAccess {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "user_id", nullable = false)
    private Long userId;

    @Column(name = "access_date", nullable = false)
    private LocalDate accessDate;

    @Column(name = "os_type", length = 20)
    private String osType;

    @CreatedDate
    @Column(name = "created_at", updatable = false)
    private LocalDateTime createdAt;

    @Builder
    public UserDailyAccess(Long userId, LocalDate accessDate, String osType) {
        this.userId = userId;
        this.accessDate = accessDate;
        this.osType = osType;
    }
}