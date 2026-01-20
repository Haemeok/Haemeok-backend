package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.ReportReason;
import jakarta.persistence.*;
import lombok.AccessLevel;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Table(name = "recipe_ingredient_report")
public class RecipeIngredientReport extends BaseTimeEntity {

    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private Long recipeId;

    @Column(nullable = false)
    private Long memberId;

    @Column(nullable = false)
    private Long ingredientId;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private ReportReason reason;

    @Column(columnDefinition = "TEXT")
    private String userMemo;

    @Column(nullable = false)
    private boolean isResolved;

    @Builder
    public RecipeIngredientReport(Long recipeId, Long ingredientId,Long memberId, ReportReason reason, String userMemo) {
        this.recipeId = recipeId;
        this.ingredientId = ingredientId;
        this.memberId = memberId;
        this.reason = reason;
        this.userMemo = userMemo;
        this.isResolved = false;
    }

    public void markAsResolved() {
        this.isResolved = true;
    }
}