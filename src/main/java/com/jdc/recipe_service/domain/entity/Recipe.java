package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.BatchSize;
import org.hibernate.annotations.Fetch;
import org.hibernate.annotations.FetchMode;

import java.math.BigDecimal;
import java.util.HashSet;
import java.util.List;
import java.util.Set;


@Entity
@Table(
        name = "recipes",
        indexes = {
                @Index(name = "idx_user_id", columnList = "user_id")
        }
)
@Getter
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@AllArgsConstructor
@Builder
public class Recipe extends BaseTimeEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @Column(length = 50, nullable = false)
    private String title;

    @Column(columnDefinition = "TEXT")
    private String description;

    @Enumerated(EnumType.STRING)
    @Column(name = "dish_type", nullable = false, length = 30)
    private DishType dishType;

    @Column(name = "cooking_time")
    private Integer cookingTime;

    @Column(name = "avg_rating", precision = 3, scale = 2)
    @Builder.Default
    private BigDecimal avgRating = BigDecimal.valueOf(0.00);

    @Column(name = "rating_count")
    @Builder.Default
    private Long ratingCount = 0L;

    @Column(name = "like_count")
    @Builder.Default
    private Long likeCount = 0L;

    @Column(name = "image_key")
    private String imageKey;

    @Column(name = "youtube_url", length = 255)
    private String youtubeUrl;

    @ElementCollection(fetch = FetchType.LAZY)
    @CollectionTable(name = "recipe_cooking_tools", joinColumns = @JoinColumn(name = "recipe_id"))
    @Column(name = "tool", length = 50)
    @BatchSize(size = 10)
    @Builder.Default
    private Set<String> cookingTools = new HashSet<>();

    @Column(name = "is_ai_generated")
    @Builder.Default
    private boolean isAiGenerated = false;

    @Column
    @Builder.Default
    private Integer servings = 1;

    @Column(nullable = false)
    @Builder.Default
    private Boolean isPrivate = false;

    @Column
    @Builder.Default
    private Integer totalIngredientCost = 0;

    @Column
    @Builder.Default
    private Integer marketPrice = 0;

    @Enumerated(EnumType.STRING)
    @Column(name = "image_status")
    private RecipeImageStatus imageStatus;

    @OneToMany(mappedBy = "recipe", fetch = FetchType.LAZY)
    @BatchSize(size = 10)
    @Builder.Default
    private Set<RecipeTag> tags = new HashSet<>();

    @OneToMany(mappedBy = "recipe", fetch = FetchType.LAZY)
    @BatchSize(size = 20)
    @Fetch(FetchMode.SUBSELECT)
    private List<RecipeIngredient> ingredients;

    @OneToMany(mappedBy = "recipe", fetch = FetchType.LAZY)
    @BatchSize(size = 100)
    @Fetch(FetchMode.SUBSELECT)
    private List<RecipeStep> steps;

    @OneToMany(mappedBy = "recipe", fetch = FetchType.LAZY)
    @BatchSize(size = 100)
    private List<RecipeLike> likes;

    @OneToOne(mappedBy = "recipe", cascade = CascadeType.ALL, orphanRemoval = true, fetch = FetchType.LAZY)
    private FineDiningDetails fineDiningDetails;

    @Column(columnDefinition = "TEXT")
    private String cookingTips;

    @Column(name = "protein", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal protein = BigDecimal.valueOf(0.00);

    @Column(name = "carbohydrate", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal carbohydrate = BigDecimal.valueOf(0.00);

    @Column(name = "fat", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal fat = BigDecimal.valueOf(0.00);

    @Column(name = "sugar", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal sugar = BigDecimal.valueOf(0.00);

    @Column(name = "sodium", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal sodium = BigDecimal.ZERO;

    @Column(name = "total_calories", precision = 10, scale = 3)
    @Builder.Default
    private BigDecimal totalCalories = BigDecimal.ZERO;

    @Column(name = "ai_analysis_status", length = 20)
    private String aiAnalysisStatus;

    public void update(String title, String description, DishType dishType, Integer cookingTime,
                       String imageKey, String youtubeUrl, Set<String> cookingTools, Integer servings,
                       Integer totalIngredientCost, Integer marketPrice, String cookingTips,
                       BigDecimal protein, BigDecimal carbohydrate, BigDecimal fat, BigDecimal sugar, BigDecimal sodium) {
        this.title = title;
        this.description = description;
        this.dishType = dishType;
        this.cookingTime = cookingTime;
        this.imageKey = imageKey;
        this.youtubeUrl = youtubeUrl;
        this.cookingTools = cookingTools;
        this.servings = servings;
        this.totalIngredientCost = totalIngredientCost;
        this.marketPrice = marketPrice;
        this.cookingTips = cookingTips;
        this.protein = protein;
        this.carbohydrate = carbohydrate;
        this.fat = fat;
        this.sugar = sugar;
        this.sodium = sodium;
    }

    public void updateImageKey(String imageKey) {
        this.imageKey = null;
        this.imageKey = imageKey;
    }

    public void updateAiGenerated(boolean isAi) {
        this.isAiGenerated = isAi;
    }

    public void updateTotalIngredientCost(Integer totalCost) {
        this.totalIngredientCost = totalCost;
    }

    public void updateMarketPrice(Integer price) {
        this.marketPrice = price;
    }

    public void updateAvgRating(BigDecimal avgRating) {
        this.avgRating = avgRating;
    }

    public void updateIsPrivate(Boolean isPrivate) {
        this.isPrivate = isPrivate;
    }

    public void updateRatingCount(Long ratingCount) {this.ratingCount = ratingCount;}

    public void updateImageStatus(RecipeImageStatus imageStatus) {this.imageStatus = imageStatus;}

    public void updateCookingTips(String cookingTips) {
        this.cookingTips = cookingTips;
    }

    public void updateNutrition(BigDecimal protein, BigDecimal carbohydrate, BigDecimal fat, BigDecimal sugar, BigDecimal sodium, BigDecimal totalCalories) {
        this.protein = protein;
        this.carbohydrate = carbohydrate;
        this.fat = fat;
        this.sugar = sugar;
        this.sodium = sodium;
        this.totalCalories = totalCalories;
    }

    public void updateAiAnalysisStatus(String status) {
        this.aiAnalysisStatus = status;
    }

    public void updateAiInfo(String cookingTips, Integer marketPrice) {
        this.cookingTips = cookingTips;
        this.marketPrice = marketPrice;
    }

    public void increaseLikeCount() {
        if (this.likeCount == null) {
            this.likeCount = 0L;
        }
        this.likeCount++;
    }
    public void decreaseLikeCount() {
        if (this.likeCount == null) {
            this.likeCount = 0L;
        }
        if (this.likeCount > 0) {
            this.likeCount--;
        }
    }

    public void setFineDiningDetails(FineDiningDetails fineDiningDetails) {
        this.fineDiningDetails = fineDiningDetails;
    }
}
