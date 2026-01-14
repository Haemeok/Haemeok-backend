package com.jdc.recipe_service.domain.dto.v2.recipe;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import com.querydsl.core.annotations.QueryProjection;
import io.swagger.v3.oas.annotations.media.Schema;
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
@Schema(description = "레시피 간략 정적 정보 DTO")
public class RecipeSimpleStaticDtoV2 {

    @Schema(description = "레시피 ID")
    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;

    @Schema(description = "레시피 제목")
    private String title;

    @Schema(description = "레시피 대표 이미지 URL")
    private String imageUrl;

    @Schema(description = "작성자 ID")
    @JsonSerialize(using = HashIdSerializer.class)
    private Long authorId;

    @Schema(description = "작성자 닉네임")
    private String authorName;

    @Schema(description = "작성자 프로필 이미지")
    private String profileImage;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
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

    @Schema(description = "레시피 원가")
    private Integer ingredientCost;

    @Schema(description = "레시피 시장가")
    private Integer marketPrice;

    @Schema(description = "유튜브 채널명")
    private String youtubeChannelName;

    @Schema(description = "유튜브 원본 영상 제목")
    private String youtubeVideoTitle;

    @Schema(description = "유튜브 썸네일 URL")
    private String youtubeThumbnailUrl;

    @Schema(description = "유튜브 레시피 여부")
    @JsonProperty("isYoutube")
    private boolean isYoutube;

    @Schema(description = "AI 생성 레시피 여부")
    @JsonProperty("isAiGenerated")
    private boolean isAiGenerated;

    @QueryProjection
    public RecipeSimpleStaticDtoV2(Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
                                   LocalDateTime createdAt, Integer cookingTime, long likeCount, BigDecimal avgRating,
                                   long ratingCount, Integer ingredientCost, Integer marketPrice,
                                   String youtubeChannelName, String youtubeVideoTitle, String youtubeThumbnailUrl,
                                   String youtubeUrl, Boolean isAiGenerated) {
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
        this.ingredientCost = ingredientCost;
        this.marketPrice = marketPrice;
        this.youtubeChannelName = youtubeChannelName;
        this.youtubeVideoTitle = youtubeVideoTitle;
        this.youtubeThumbnailUrl = youtubeThumbnailUrl;
        this.isYoutube = youtubeUrl != null && !youtubeUrl.isEmpty();
        this.isAiGenerated = isAiGenerated != null && isAiGenerated;
    }

    public RecipeSimpleStaticDtoV2(Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
                                   LocalDateTime createdAt, Integer cookingTime, Long likeCount,
                                   Double avgRating, Long ratingCount, Integer ingredientCost, Integer marketPrice,
                                   String youtubeChannelName, String youtubeVideoTitle, String youtubeThumbnailUrl,
                                   String youtubeUrl, Boolean isAiGenerated) {
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
        this.ingredientCost = ingredientCost;
        this.marketPrice = marketPrice;
        this.youtubeChannelName = youtubeChannelName;
        this.youtubeVideoTitle = youtubeVideoTitle;
        this.youtubeThumbnailUrl = youtubeThumbnailUrl;
        this.isYoutube = youtubeUrl != null && !youtubeUrl.isEmpty();
        this.isAiGenerated = isAiGenerated != null && isAiGenerated;
    }
}