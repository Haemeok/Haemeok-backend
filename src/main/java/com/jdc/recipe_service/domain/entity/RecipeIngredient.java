package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.dto.report.AdminIngredientUpdateDto;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;

@Entity
@Table(name = "recipe_ingredients",
        indexes = {
                @Index(name = "idx_ri_covering", columnList = "ingredient_id, recipe_id")
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

    @Column(name = "ingredient_unit_id")
    private Long ingredientUnitId;

    @Column(name = "ingredient_candidate_id")
    private Long ingredientCandidateId;

    @Column(name = "raw_name", length = 100)
    private String rawName;

    @Column(name = "raw_quantity_text", length = 50)
    private String rawQuantityText;

    @Column(name = "raw_unit_text", length = 50)
    private String rawUnitText;

    @Column(name = "amount_value", precision = 12, scale = 3)
    private BigDecimal amountValue;

    @Column(name = "normalized_grams", precision = 12, scale = 3)
    private BigDecimal normalizedGrams;

    @Column(name = "resolution_status", length = 20)
    private String resolutionStatus;

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

    @Column(name = "custom_link", length = 2048)
    private String customLink;

    public void updateWithMapping(String name, String quantity, String unit,
                                  Ingredient master, Integer calculatedPrice,
                                  AdminIngredientUpdateDto dto) {
        this.rawName = name;
        this.rawQuantityText = quantity;
        this.rawUnitText = unit;
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

    public void updateCustomLink(String link) {
        this.customLink = link;
    }

    /**
     * 2차 백필 전용 진입점 — 4개 정규화 필드만 갱신.
     *
     * <p>quantity/unit/customName/customUnit/custom_(price/calorie/etc)/raw_(name/quantity_text/unit_text)는
     * 절대 건드리지 않는다 (raw 보존 정책). 이 메서드를 통해서만 갱신하게 함으로써 setter 노출 없이
     * invariant를 강제.
     */
    public void applyNormalizationBackfill(java.math.BigDecimal amountValue,
                                            Long ingredientUnitId,
                                            java.math.BigDecimal normalizedGrams,
                                            String resolutionStatus) {
        this.amountValue = amountValue;
        this.ingredientUnitId = ingredientUnitId;
        this.normalizedGrams = normalizedGrams;
        this.resolutionStatus = resolutionStatus;
    }
}
