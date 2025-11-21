package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "recipe_trash_log")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RecipeTrashLog {

    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private Long originalRecipeId;
    private Long userId;

    @Column(length = 100)
    private String title;

    @Column(columnDefinition = "TEXT")
    private String ingredientsSnapshot;

    @Column(columnDefinition = "TEXT")
    private String instructionSnapshot;

    private String detectedReason;

    private LocalDateTime deletedAt;

    @PrePersist
    public void prePersist() {
        this.deletedAt = LocalDateTime.now();
    }
}