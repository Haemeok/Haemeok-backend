package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseCreateTimeEntity;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Table(name = "ingredient_candidates")
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class IngredientCandidate extends BaseCreateTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "candidate_type", nullable = false, length = 20)
    private String candidateType;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ingredient_id")
    private Ingredient ingredient;

    @Column(name = "raw_name", length = 100)
    private String rawName;

    @Column(name = "raw_unit_text", length = 50)
    private String rawUnitText;

    @Column(name = "proposed_name_ko", length = 100)
    private String proposedNameKo;

    @Column(name = "proposed_unit_label_ko", length = 50)
    private String proposedUnitLabelKo;

    @Column(name = "proposed_grams_per_unit", precision = 12, scale = 3)
    private BigDecimal proposedGramsPerUnit;

    @Column(name = "proposed_edible_grams_per_unit", precision = 12, scale = 3)
    private BigDecimal proposedEdibleGramsPerUnit;

    @Column(name = "source_recipe_id")
    private Long sourceRecipeId;

    @Column(name = "source_ref", length = 255)
    private String sourceRef;

    @Column(name = "status", nullable = false, length = 20)
    private String status;

    @Column(name = "reviewed_at")
    private LocalDateTime reviewedAt;
}
