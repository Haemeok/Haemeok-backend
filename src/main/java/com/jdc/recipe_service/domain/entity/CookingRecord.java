package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseCreateTimeEntity;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;

@Entity
@Table(
        name = "cooking_records",
        indexes = {
                @Index(name = "idx_user_date", columnList = "user_id, created_at")
        }
)
@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
public class CookingRecord extends BaseCreateTimeEntity {

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

}