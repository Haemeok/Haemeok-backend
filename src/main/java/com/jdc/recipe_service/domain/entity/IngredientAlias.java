package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseCreateTimeEntity;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(
        name = "ingredient_aliases",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"ingredient_id", "normalized_alias"})
        },
        indexes = {
                @Index(name = "idx_alias_normalized", columnList = "normalized_alias")
        }
)
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class IngredientAlias extends BaseCreateTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ingredient_id", nullable = false)
    private Ingredient ingredient;

    @Column(name = "alias_text", nullable = false, length = 100)
    private String aliasText;

    @Column(name = "normalized_alias", nullable = false, length = 100)
    private String normalizedAlias;
}
