package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseCreateTimeEntity;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;

@Entity
@Table(
        name = "ingredient_units",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"ingredient_id", "normalized_unit_label"})
        }
)
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class IngredientUnit extends BaseCreateTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ingredient_id", nullable = false)
    private Ingredient ingredient;

    @Column(name = "unit_label_ko", nullable = false, length = 50)
    private String unitLabelKo;

    @Column(name = "normalized_unit_label", nullable = false, length = 50)
    private String normalizedUnitLabel;

    @Column(name = "grams_per_unit", nullable = false, precision = 12, scale = 3)
    private BigDecimal gramsPerUnit;

    @Column(name = "edible_grams_per_unit", nullable = false, precision = 12, scale = 3)
    private BigDecimal edibleGramsPerUnit;

    @Column(name = "is_default", nullable = false)
    @Builder.Default
    private Boolean isDefault = false;
}
