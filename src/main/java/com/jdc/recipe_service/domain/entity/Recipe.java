package com.jdc.recipe_service.domain.entity;

import com.jdc.recipe_service.domain.entity.common.BaseTimeEntity;
import jakarta.persistence.*;
import lombok.*;
import java.math.BigDecimal;


@Entity
@Table(
        name = "recipes",
        indexes = {
                @Index(name = "idx_user_id", columnList = "user_id")
        }
)
@Getter
@Setter
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

    @Column(name = "dish_type", length = 50)
    private String dishType;

    @Column(name = "cooking_time")
    private Integer cookingTime;

    @Column(name = "avg_rating", precision = 3, scale = 2)
    private BigDecimal avgRating = BigDecimal.valueOf(0.00);

    @Column(name = "image_url", length = 255)
    private String imageUrl;

    @Column(name = "youtube_url", length = 255)
    private String youtubeUrl;

    @Column(name = "cooking_tools", columnDefinition = "TEXT")
    private String cookingTools;

    @Column(name = "is_ai_generated", nullable = false)
    private boolean isAiGenerated = false;

    // 도메인 메서드: 레시피 업데이트
    public void updateRecipe(String title, String description, String dishType, Integer cookingTime, String imageUrl, String youtubeUrl, String cookingTools) {
        this.title = title;
        this.description = description;
        this.dishType = dishType;
        this.cookingTime = cookingTime;
        this.imageUrl = imageUrl;
        this.youtubeUrl = youtubeUrl;
        this.cookingTools = cookingTools;
    }
}
