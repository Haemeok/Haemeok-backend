package com.jdc.recipe_service.domain.dto.v2.recipe;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.querydsl.core.annotations.QueryProjection;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@Schema(description = "레시피 간략 정적 정보 DTO")
public class RecipeSimpleStaticDto {

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

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    @Schema(description = "생성일시 (UTC)")
    private LocalDateTime createdAt;

    @Schema(description = "예상 조리 시간 (분 단위)")
    private Integer cookingTime;

    @Schema(description = "레시피 좋아요 수")
    private long likeCount;

    @Schema(description = "레시피 평점 평균")
    private double avgRating;

    @Schema(description = "레시피 평점 수")
    private long ratingCount;

    @QueryProjection
    public RecipeSimpleStaticDto(Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
                                 LocalDateTime createdAt, Integer cookingTime, long likeCount,
                                 BigDecimal avgRating,
                                 long ratingCount) {
        this.id = id;
        this.title = title;
        this.imageUrl = imageUrl;
        this.authorId = authorId;
        this.authorName = authorName;
        this.profileImage = profileImage;
        this.createdAt = createdAt;
        this.cookingTime = cookingTime;
        this.likeCount = likeCount;
        this.avgRating = avgRating != null ? avgRating.doubleValue() : 0.0;
        this.ratingCount = ratingCount;
    }

    public RecipeSimpleStaticDto(Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
                                 LocalDateTime createdAt, Integer cookingTime,
                                 Long likeCount,
                                 Double avgRating,
                                 Long ratingCount)
    {
        this.id = id;
        this.title = title;
        this.imageUrl = imageUrl;
        this.authorId = authorId;
        this.authorName = authorName;
        this.profileImage = profileImage;
        this.createdAt = createdAt;
        this.cookingTime = cookingTime;
        this.likeCount = likeCount != null ? likeCount : 0L;
        this.avgRating = avgRating != null ? avgRating : 0.0;
        this.ratingCount = ratingCount != null ? ratingCount : 0L;
    }
}