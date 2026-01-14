package com.jdc.recipe_service.domain.dto.recipe;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
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

    public void setLikedByCurrentUser(boolean b) {
        likedByCurrentUser = b;
    }

    public void setImageUrl(String imageUrl) {
        this.imageUrl = imageUrl;
    }

    @QueryProjection
    public RecipeSimpleDto(Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
                           LocalDateTime createdAt, Long likeCount, boolean likedByCurrentUser, Integer cookingTime,
                           String youtubeChannelName, String youtubeVideoTitle, String youtubeThumbnailUrl,
                           BigDecimal avgRating, Long ratingCount, String youtubeUrl, Boolean isAiGenerated) {
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
        this.youtubeChannelName = youtubeChannelName;
        this.youtubeVideoTitle = youtubeVideoTitle;
        this.youtubeThumbnailUrl = youtubeThumbnailUrl;
        this.avgRating = avgRating;
        this.ratingCount = ratingCount;
        this.isYoutube = youtubeUrl != null && !youtubeUrl.isEmpty();
        this.isAiGenerated = isAiGenerated != null && isAiGenerated;
    }

    public RecipeSimpleDto(Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
                           LocalDateTime createdAt, Long likeCount, boolean likedByCurrentUser, Integer cookingTime,
                           String youtubeChannelName, String youtubeVideoTitle, String youtubeThumbnailUrl,
                           double avgRating, Long ratingCount, String youtubeUrl, Boolean isAiGenerated) {
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
        this.youtubeChannelName = youtubeChannelName;
        this.youtubeVideoTitle = youtubeVideoTitle;
        this.youtubeThumbnailUrl = youtubeThumbnailUrl;
        this.avgRating = BigDecimal.valueOf(avgRating);
        this.ratingCount = ratingCount != null ? ratingCount : 0L;
        this.isYoutube = youtubeUrl != null && !youtubeUrl.isEmpty();
        this.isAiGenerated = isAiGenerated != null && isAiGenerated;
    }

    public Double getAvgRating() {
        return avgRating != null ? avgRating.doubleValue() : 0.0;
    }
}
