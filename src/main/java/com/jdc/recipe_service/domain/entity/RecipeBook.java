package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "recipe_books",
        indexes = @Index(name = "idx_recipe_books_user_order",
                columnList = "user_id, display_order"))
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RecipeBook extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @Column(nullable = false, length = 50)
    private String name;

    @Column(name = "is_default", nullable = false)
    @Builder.Default
    private boolean isDefault = false;

    @Column(name = "display_order", nullable = false)
    @Builder.Default
    private int displayOrder = 0;

    @Column(name = "recipe_count", nullable = false)
    @Builder.Default
    private int recipeCount = 0;

    public void rename(String newName) {
        this.name = newName;
    }

    public void updateDisplayOrder(int order) {
        this.displayOrder = order;
    }

    public void incrementRecipeCount() {
        this.recipeCount++;
    }

    public void decrementRecipeCount(int count) {
        this.recipeCount = Math.max(0, this.recipeCount - count);
    }
}
