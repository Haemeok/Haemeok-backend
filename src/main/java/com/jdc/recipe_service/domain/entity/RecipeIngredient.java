package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.dto.report.AdminIngredientUpdateDto;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;

@Entity
@Table(name = "recipe_ingredients",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"recipe_id", "ingredient_id"})
        },
        indexes = {
                @Index(name = "idx_recipe_ingredient_ing_id", columnList = "ingredient_id")
        }
)
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

    @Column(name = "custom_calorie", precision=10, scale=3)
    @Builder.Default
    private BigDecimal customCalorie = BigDecimal.ZERO;

    @Column(name = "custom_carbohydrate", precision=10, scale=3)
    @Builder.Default
    private BigDecimal customCarbohydrate = BigDecimal.ZERO;

    @Column(name = "custom_protein", precision=10, scale=3)
    @Builder.Default
    private BigDecimal customProtein = BigDecimal.ZERO;

    @Column(name = "custom_fat", precision=10, scale=3)
    @Builder.Default
    private BigDecimal customFat = BigDecimal.ZERO;

    @Column(name = "custom_sugar", precision=10, scale=3)
    @Builder.Default
    private BigDecimal customSugar = BigDecimal.ZERO;

    @Column(name = "custom_sodium", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal customSodium = BigDecimal.ZERO;

    public void updateWithMapping(String name, String quantity, String unit,
                                  Ingredient master, Integer calculatedPrice,
                                  AdminIngredientUpdateDto dto) {
        this.quantity = quantity;
        this.unit = unit;
        this.price = calculatedPrice;

        if (master != null) {
            this.ingredient = master;
            this.customName = null;
            this.customUnit = null;
            this.customPrice = 0;
            this.customCalorie = BigDecimal.ZERO;
            this.customCarbohydrate = BigDecimal.ZERO;
            this.customProtein = BigDecimal.ZERO;
            this.customFat = BigDecimal.ZERO;
            this.customSugar = BigDecimal.ZERO;
            this.customSodium = BigDecimal.ZERO;
        } else {
            this.ingredient = null;
            this.customName = name;
            this.customUnit = unit;
            this.customPrice = calculatedPrice;

            this.customCalorie = dto.getCalorie() != null ? dto.getCalorie() : BigDecimal.ZERO;
            this.customCarbohydrate = dto.getCarbohydrate() != null ? dto.getCarbohydrate() : BigDecimal.ZERO;
            this.customProtein = dto.getProtein() != null ? dto.getProtein() : BigDecimal.ZERO;
            this.customFat = dto.getFat() != null ? dto.getFat() : BigDecimal.ZERO;
            this.customSugar = dto.getSugar() != null ? dto.getSugar() : BigDecimal.ZERO;
            this.customSodium = dto.getSodium() != null ? dto.getSodium() : BigDecimal.ZERO;
        }
    }
}
