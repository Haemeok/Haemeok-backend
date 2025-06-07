package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;

import java.util.ArrayList;
import java.util.List;

@Entity
@Table(name = "recipe_steps", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"recipe_id", "step_number"})
})
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RecipeStep {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "recipe_id", nullable = false)
    private Recipe recipe;

    @Column(name = "step_number", nullable = false)
    private Integer stepNumber;

    @Column(columnDefinition = "TEXT")
    private String instruction;

    @Column(name = "step_image_key")
    private String imageKey;

    @Column(length = 50)
    private String action;

    @OneToMany(mappedBy = "step", fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @BatchSize(size = 10)
    @Fetch(FetchMode.SUBSELECT)
    @Builder.Default
    private List<RecipeStepIngredient> stepIngredients = new ArrayList<>();

    public void updateInstruction(String instruction) {
        this.instruction = instruction;
    }

    public void updateStepImageKey(String imageKey) {
        this.imageKey = imageKey != null && !imageKey.isBlank() ? imageKey : null;
    }

    public void updateAction(String action) {
        this.action = action;
    }
}
