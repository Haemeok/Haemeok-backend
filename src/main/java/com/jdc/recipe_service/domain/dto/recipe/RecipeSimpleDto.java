package com.jdc.recipe_service.domain.dto.recipe;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.querydsl.core.annotations.QueryProjection;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@Schema(description = "레시피 간략 정보 DTO")
public class RecipeSimpleDto {

    @Schema(description = "레시피 ID")
    private Long id;

    @Schema(description = "레시피 제목")
    private String title;

    @Schema(description = "레시피 대표 이미지 URL")
    private String imageUrl;

    @Schema(description = "작성자 ID")
    private Long authorId;

    @Schema(description = "작성자 닉네임")
    private String authorName;

    @Schema(description = "작성자 프로필 이미지")
    private String profileImage;

    @JsonFormat(
            shape = JsonFormat.Shape.STRING,
            pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'",
            timezone = "UTC"
    )
    @Schema(description = "생성일시 (UTC)")
    private LocalDateTime createdAt;

    @Schema(description = "좋아요 수")
    private Long likeCount;

    @Schema(description = "현재 로그인한 사용자가 좋아요를 눌렀는지 여부")
    private boolean likedByCurrentUser;

    @Schema(description = "평균 평점")
    @Getter(AccessLevel.NONE)
    private BigDecimal avgRating;

    @Schema(description = "평점 참여 수")
    private Long ratingCount;

    @Schema(description = "예상 조리 시간 (분 단위)")
    private Integer cookingTime;

    public void setLikedByCurrentUser(boolean b) {
        likedByCurrentUser = b;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    @QueryProjection
    public RecipeSimpleDto(Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
                           LocalDateTime createdAt, Long likeCount, boolean likedByCurrentUser,
                           Integer cookingTime, BigDecimal avgRating, Long ratingCount) {
        this.id = id;
        this.title = title;
        this.imageUrl = imageUrl;
        this.authorId = authorId;
        this.authorName = authorName;
        this.profileImage = profileImage;
        this.createdAt = createdAt;
        this.likeCount = likeCount;
        this.likedByCurrentUser = likedByCurrentUser;
        this.cookingTime = cookingTime;
        this.avgRating = avgRating;
        this.ratingCount = ratingCount;
    }

    public RecipeSimpleDto(Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
                           LocalDateTime createdAt, Long likeCount, boolean likedByCurrentUser,
                           Integer cookingTime, double avgRating, Long ratingCount) {
        this.id = id;
        this.title = title;
        this.imageUrl = imageUrl;
        this.authorId = authorId;
        this.authorName = authorName;
        this.profileImage = profileImage;
        this.createdAt = createdAt;
        this.likeCount = likeCount;
        this.likedByCurrentUser = likedByCurrentUser;
        this.cookingTime = cookingTime;
        this.avgRating = BigDecimal.valueOf(avgRating);
        this.ratingCount = ratingCount != null ? ratingCount : 0L;
    }

    public Double getAvgRating() {
        return avgRating != null ? avgRating.doubleValue() : 0.0;
    }
}
