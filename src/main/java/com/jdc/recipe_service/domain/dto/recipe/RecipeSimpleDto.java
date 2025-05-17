package com.jdc.recipe_service.domain.dto.recipe;


import com.fasterxml.jackson.annotation.JsonFormat;
import com.querydsl.core.annotations.QueryProjection;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor

/**
 * 레시피 단순 조히용
 */
public class RecipeSimpleDto {
    private Long id;
    private String title;
    private String imageUrl;
    private String authorName;
    private String profileImage;
    @JsonFormat(
            shape = JsonFormat.Shape.STRING,
            pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'",
            timezone = "UTC"
    )
    private LocalDateTime createdAt;

    private long likeCount;
    private boolean likedByCurrentUser;

    private BigDecimal avgRating;
    private Long ratingCount;
    private Integer cookingTime;

    public void setLikedByCurrentUser(boolean b) {
        likedByCurrentUser = b;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    @QueryProjection
    public RecipeSimpleDto(Long id, String title, String imageUrl, String authorName, String profileImage,
                           LocalDateTime createdAt, long likeCount, boolean likedByCurrentUser,
                           Integer cookingTime,
                           BigDecimal avgRating, Long ratingCount) {
        this.id = id;
        this.title = title;
        this.imageUrl = imageUrl;
        this.authorName = authorName;
        this.profileImage = profileImage;
        this.createdAt = createdAt;
        this.likeCount = likeCount;
        this.likedByCurrentUser = likedByCurrentUser;
        this.cookingTime = cookingTime;
        this.avgRating = avgRating;
        this.ratingCount = ratingCount;
    }

    public RecipeSimpleDto(Long id, String title, String imageUrl, String authorName, String profileImage,
                           LocalDateTime createdAt, long likeCount, boolean likedByCurrentUser,
                           Integer cookingTime, double avgRating, Long ratingCount) {
        this.id = id;
        this.title = title;
        this.imageUrl = imageUrl;
        this.authorName = authorName;
        this.profileImage = profileImage;
        this.createdAt = createdAt;
        this.likeCount = likeCount;
        this.likedByCurrentUser = likedByCurrentUser;
        this.cookingTime = cookingTime;
        this.avgRating = BigDecimal.valueOf(avgRating);
        this.ratingCount = ratingCount != null ? ratingCount : 0L;
    }




}
