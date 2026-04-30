package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Table(name = "ingredient_price_snapshots")
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class IngredientPriceSnapshot {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ingredient_id", nullable = false)
    private Ingredient ingredient;

    @Column(name = "source_name", nullable = false, length = 50)
    private String sourceName;

    @Column(name = "source_label", length = 255)
    private String sourceLabel;

    @Column(name = "source_link", columnDefinition = "TEXT")
    private String sourceLink;

    @Column(name = "price_per_g", nullable = false, precision = 12, scale = 4)
    private BigDecimal pricePerG;

    @Column(name = "captured_at", nullable = false)
    private LocalDateTime capturedAt;
}
