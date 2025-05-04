package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import com.jdc.recipe_service.domain.type.DishType;
import jakarta.persistence.*;
import lombok.*;
import org.hibernate.annotations.BatchSize;

import java.math.BigDecimal;
import java.util.List;


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

    // 레시피 작성자를 단방향 ManyToOne으로 참조
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

    @Column(name = "image_key")
    private String imageKey;

    @Column(name = "youtube_url", length = 255)
    private String youtubeUrl;

    @ElementCollection
    @CollectionTable(name = "recipe_cooking_tools", joinColumns = @JoinColumn(name = "recipe_id"))
    @Column(name = "tool", length = 50)
    @BatchSize(size = 10)
    private List<String> cookingTools;

    @Column(name = "is_ai_generated")
    @Builder.Default
    private boolean isAiGenerated = false;

    @Column
    @Builder.Default
    private Integer servings = 1; // 기본값 1인분

    @Column(nullable = false)
    @Builder.Default
    private Boolean isPrivate = false; // 기본값 공개

    @Column
    @Builder.Default
    private Integer totalIngredientCost = 0; // 재료 기준 가격

    @Column
    @Builder.Default
    private Integer marketPrice = 0; // 시중 평균 가격


    @OneToMany(mappedBy = "recipe", fetch = FetchType.LAZY)
    @BatchSize(size = 10)
    private List<RecipeTag> tags;

    @OneToMany(mappedBy = "recipe", fetch = FetchType.LAZY)
    @BatchSize(size = 20)
    private List<RecipeIngredient> ingredients;

    @OneToMany(mappedBy = "recipe", fetch = FetchType.LAZY)
    @BatchSize(size = 10)
    private List<RecipeStep> steps;

    @OneToMany(mappedBy = "recipe", fetch = FetchType.LAZY)
    @BatchSize(size = 100)
    private List<RecipeLike> likes;

    public void update(String title, String description, DishType dishType, Integer cookingTime,
                       String imageKey, String youtubeUrl, List<String> cookingTools, Integer servings,
                       Integer totalIngredientCost, Integer marketPrice) {
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
       // this.isAiGenerated = false;
    }

    public void updateImageKey(String imageKey) {
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
}
