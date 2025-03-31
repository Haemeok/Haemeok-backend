package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.*;

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

    @Column(columnDefinition = "TEXT", nullable = false)
    private String instruction;

    @Column(name = "step_image_url", length = 255)
    private String stepImageUrl;

    @Column(name = "cooking_tools", columnDefinition = "TEXT")
    private String cookingTools;

    @Column(length = 50)
    private String action;
}
