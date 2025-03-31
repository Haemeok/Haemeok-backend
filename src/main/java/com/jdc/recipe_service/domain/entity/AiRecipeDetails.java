package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "ai_recipe_details")
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class AiRecipeDetails {

    @Id
    private Long recipeId;

    @OneToOne(fetch = FetchType.LAZY)
    @MapsId
    @JoinColumn(name = "recipe_id")
    private Recipe recipe;

    @Column(name = "creator_comment", columnDefinition = "TEXT")
    private String creatorComment;

    @Column(name = "ai_generated_at")
    private LocalDateTime aiGeneratedAt;

    @Column(name = "ai_prompt", columnDefinition = "TEXT")
    private String aiPrompt;

    @PrePersist
    protected void onCreate() {
        this.aiGeneratedAt = LocalDateTime.now();
    }

    // 도메인 메서드: AI 정보 업데이트
    public void updateAiDetails(String creatorComment, String aiPrompt) {
        this.creatorComment = creatorComment;
        this.aiPrompt = aiPrompt;
    }
}
