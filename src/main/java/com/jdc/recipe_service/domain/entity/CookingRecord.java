package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.CreationTimestamp;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Table(
        name = "cooking_records",
        indexes = {
                @Index(name = "idx_user_date", columnList = "user_id, created_at")
        }
)
@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
public class CookingRecord {

    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "recipe_id", nullable = false)
    private Recipe recipe;

    @Column(name = "ingredient_cost", nullable = false)
    private Integer ingredientCost;

    @Column(name = "market_price", nullable = false)
    private Integer marketPrice;

    @Column(nullable = false)
    private Integer savings;

    @Column(name = "protein", precision = 10, scale = 3)
    private BigDecimal protein;

    @Column(name = "carbohydrate", precision = 10, scale = 3)
    private BigDecimal carbohydrate;

    @Column(name = "fat", precision = 10, scale = 3)
    private BigDecimal fat;

    @Column(name = "sugar", precision = 10, scale = 3)
    private BigDecimal sugar;

    @Column(name = "sodium", precision = 10, scale = 3)
    private BigDecimal sodium;

    @Column(name = "total_calories", precision = 10, scale = 3)
    private BigDecimal totalCalories;

    @CreationTimestamp
    @Column(name = "created_at", nullable = false, updatable = false)
    private LocalDateTime createdAt;
}