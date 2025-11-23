package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;

@Entity
@Table(name = "recipe_ingredients", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"recipe_id", "ingredient_id"})
})
@Getter
@ToString(exclude = "recipe")
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class RecipeIngredient {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "recipe_id", nullable = false)
    private Recipe recipe;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ingredient_id")
    private Ingredient ingredient;

    @Column(length = 50)
    private String quantity;

    @Column(length = 20)
    private String unit;

    @Column(name = "price")
    @Builder.Default
    private Integer price = 0;

    private String customName;
    private String customUnit;


    @Column(name = "custom_price")
    @Builder.Default
    private Integer customPrice = 0;

    @Column(name = "custom_calorie", precision=10, scale=2)
    @Builder.Default
    private BigDecimal customCalorie = BigDecimal.ZERO;

    @Column(name = "custom_carbohydrate", precision=10, scale=2)
    @Builder.Default
    private BigDecimal customCarbohydrate = BigDecimal.ZERO;

    @Column(name = "custom_protein", precision=10, scale=2)
    @Builder.Default
    private BigDecimal customProtein = BigDecimal.ZERO;

    @Column(name = "custom_fat", precision=10, scale=2)
    @Builder.Default
    private BigDecimal customFat = BigDecimal.ZERO;

    @Column(name = "custom_sugar", precision=10, scale=2)
    @Builder.Default
    private BigDecimal customSugar = BigDecimal.ZERO;

    @Column(name = "custom_sodium", precision = 10, scale = 2)
    @Builder.Default
    private BigDecimal customSodium = BigDecimal.ZERO;
}
