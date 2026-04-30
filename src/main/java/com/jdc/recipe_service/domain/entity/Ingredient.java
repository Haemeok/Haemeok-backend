package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.entity.converter.IntegerListConverter;
import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "ingredients", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"name"})
})
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class Ingredient extends BaseTimeEntity {

    @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false, length = 100)
    private String name;

    @Column(length = 50)
    private String category;

    @Column(name = "image_url", nullable = true, length = 255)
    private String imageUrl;

    @Column(name = "price")
    private Integer price;

    @Column
    private String unit;

    @Column(name = "calorie", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal calorie = BigDecimal.ZERO;

    @Column(name = "english_name", length = 100)
    private String englishName;

    @Column(name = "coupang_link", length = 2048, nullable = true)
    private String coupangLink;

    @Column(name = "coupang_link_updated_at", nullable = true)
    private LocalDateTime coupangLinkUpdatedAt;

    @Column(name = "carbohydrate", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal carbohydrate = BigDecimal.ZERO;

    @Column(name = "protein", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal protein = BigDecimal.ZERO;

    @Column(name = "fat", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal fat = BigDecimal.ZERO;

    @Column(name = "sugar", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal sugar = BigDecimal.ZERO;

    @Column(name = "sodium", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal sodium = BigDecimal.ZERO;

    @Column(name = "usage_count")
    @Builder.Default
    private Long usageCount = 0L;

    @Column(name = "kcal_per_g", precision = 12, scale = 6)
    private BigDecimal kcalPerG;

    @Column(name = "carbohydrate_g_per_g", precision = 12, scale = 6)
    private BigDecimal carbohydrateGPerG;

    @Column(name = "protein_g_per_g", precision = 12, scale = 6)
    private BigDecimal proteinGPerG;

    @Column(name = "fat_g_per_g", precision = 12, scale = 6)
    private BigDecimal fatGPerG;

    @Column(name = "sugar_g_per_g", precision = 12, scale = 6)
    private BigDecimal sugarGPerG;

    @Column(name = "sodium_mg_per_g", precision = 12, scale = 6)
    private BigDecimal sodiumMgPerG;

    @Column(name = "price_per_g", precision = 12, scale = 4)
    private BigDecimal pricePerG;

    @Column(name = "price_snapshot_id")
    private Long priceSnapshotId;

    @Column(name = "is_active")
    @Builder.Default
    private Boolean isActive = true;

    @Column(name = "is_pantry")
    @Builder.Default
    private boolean isPantry = false;

    @Column(name = "storage_location", length = 50)
    private String storageLocation;

    @Column(name = "storage_temperature", length = 255)
    private String storageTemperature;

    @Column(name = "storage_duration", length = 255)
    private String storageDuration;

    @Column(name = "storage_notes", columnDefinition = "TEXT")
    private String storageNotes;

    @Column(name = "good_pairs", columnDefinition = "TEXT")
    private String goodPairs;

    @Column(name = "bad_pairs", columnDefinition = "TEXT")
    private String badPairs;

    @Column(name = "benefits", columnDefinition = "TEXT")
    private String benefits;

    @Convert(converter = IntegerListConverter.class)
    @Column(name = "season_months", length = 64)
    private List<Integer> seasonMonths;

    @Column(name = "recommended_cooking_methods", columnDefinition = "TEXT")
    private String recommendedCookingMethods;

    public void updateUsageCount(Long count) {
        this.usageCount = count;
    }

    public void updateCoupangLink(String link) {
        this.coupangLink = link;
        this.coupangLinkUpdatedAt = LocalDateTime.now();
    }

    public void markAsPantry() {
        this.isPantry = true;
    }

}
