package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.type.ImageStatus;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RecipeImage {

    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    private Recipe recipe;

    private String slot; // 예: main, step_0, step_1

    private String fileKey;

    @Enumerated(EnumType.STRING)
    private ImageStatus status;

    public void updateStatusToActive() {
        this.status = ImageStatus.ACTIVE;
    }
}

