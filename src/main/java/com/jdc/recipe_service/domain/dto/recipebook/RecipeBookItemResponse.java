package com.jdc.recipe_service.domain.dto.recipebook;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeBookItem;
import com.jdc.recipe_service.domain.entity.User;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "레시피북 내 레시피 항목")
public class RecipeBookItemResponse {

    @JsonSerialize(using = HashIdSerializer.class)
    @Schema(description = "레시피 ID")
    private Long recipeId;

    @Schema(description = "레시피 제목")
    private String title;

    @Schema(description = "레시피 이미지 URL")
    private String imageUrl;

    @Schema(description = "요리 유형")
    private String dishType;

    @JsonSerialize(using = HashIdSerializer.class)
    @Schema(description = "작성자 ID")
    private Long authorId;

    @Schema(description = "작성자 닉네임")
    private String authorName;

    @Schema(description = "작성자 프로필 이미지")
    private String profileImage;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
    @Schema(description = "레시피북에 추가된 시간")
    private LocalDateTime addedAt;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
    @Schema(description = "레시피 생성 시간")
    private LocalDateTime createdAt;

    @Schema(description = "즐겨찾기 수", example = "12")
    private Long favoriteCount;

    @Schema(description = "조리 시간(분)", example = "20")
    private Integer cookingTime;

    @Schema(description = "유튜브 채널명")
    private String youtubeChannelName;

    @Schema(description = "유튜브 채널 ID")
    private String youtubeChannelId;

    @Schema(description = "유튜브 영상 제목")
    private String youtubeVideoTitle;

    @Schema(description = "유튜브 영상 썸네일 URL")
    private String youtubeThumbnailUrl;

    @Schema(description = "유튜브 채널 프로필 URL")
    private String youtubeChannelProfileUrl;

    @Schema(description = "유튜브 구독자 수")
    private Long youtubeSubscriberCount;

    @Schema(description = "유튜브 영상 조회 수")
    private Long youtubeVideoViewCount;

    @Schema(description = "유튜브에서 가져온 레시피 여부", example = "false")
    private boolean isYoutube;

    @Schema(description = "AI가 생성한 레시피 여부", example = "false")
    private boolean isAiGenerated;

    public static RecipeBookItemResponse from(RecipeBookItem item, String imageUrl) {
        Recipe recipe = item.getRecipe();
        User author = recipe.getUser();
        String youtubeUrl = recipe.getYoutubeUrl();
        boolean isYoutube = youtubeUrl != null && !youtubeUrl.isEmpty();

        return RecipeBookItemResponse.builder()
                .recipeId(recipe.getId())
                .title(recipe.getTitle())
                .imageUrl(imageUrl)
                .dishType(recipe.getDishType() != null ? recipe.getDishType().name() : null)
                .authorId(author != null ? author.getId() : null)
                .authorName(author != null ? author.getNickname() : null)
                .profileImage(author != null ? author.getProfileImage() : null)
                .addedAt(item.getCreatedAt())
                .createdAt(recipe.getCreatedAt())
                .favoriteCount(recipe.getFavoriteCount())
                .cookingTime(recipe.getCookingTime())
                .youtubeChannelName(recipe.getYoutubeChannelName())
                .youtubeChannelId(recipe.getYoutubeChannelId())
                .youtubeVideoTitle(recipe.getYoutubeVideoTitle())
                .youtubeThumbnailUrl(recipe.getYoutubeThumbnailUrl())
                .youtubeChannelProfileUrl(recipe.getYoutubeChannelProfileUrl())
                .youtubeSubscriberCount(recipe.getYoutubeSubscriberCount())
                .youtubeVideoViewCount(recipe.getYoutubeVideoViewCount())
                .isYoutube(isYoutube)
                .isAiGenerated(recipe.isAiGenerated())
                .build();
    }
}
