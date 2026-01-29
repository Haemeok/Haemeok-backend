package com.jdc.recipe_service.domain.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Entity
@Table(name = "ingredients", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"name"})
})
@Getter
@Setter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class Ingredient {

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

    @Column(name = "coupang_link", length = 1024, nullable = true)
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

    public void updateUsageCount(Long count) {
        this.usageCount = count;
    }

}
