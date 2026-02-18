package com.jdc.recipe_service.domain.entity.recipe;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.JobStatus;
import com.jdc.recipe_service.domain.type.JobType;
import com.jdc.recipe_service.domain.type.recipe.RecipeDisplayMode;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
@Table(name = "recipe_generation_jobs", indexes = {
        @Index(name = "idx_job_user_created", columnList = "userId, created_at"),
        @Index(name = "idx_job_idempotency", columnList = "idempotencyKey")
})
public class RecipeGenerationJob extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private Long userId;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private JobType jobType;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private JobStatus status;

    private Long resultRecipeId;

    @Column(columnDefinition = "TEXT")
    private String errorMessage;

    @Column(nullable = false)
    private int progress;

    @Column(unique = true)
    private String idempotencyKey;

    @Enumerated(EnumType.STRING)
    @Column(name = "display_mode")
    private RecipeDisplayMode displayMode;

    public void updateProgress(JobStatus status, int progress) {
        this.status = status;
        this.progress = progress;
    }

    public void complete(Long recipeId) {
        this.status = JobStatus.COMPLETED;
        this.progress = 100;
        this.resultRecipeId = recipeId;
    }

    public void fail(String message) {
        this.status = JobStatus.FAILED;
        this.progress = 0;
        this.errorMessage = message;
    }
}
