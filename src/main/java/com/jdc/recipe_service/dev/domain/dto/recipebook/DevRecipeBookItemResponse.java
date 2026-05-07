package com.jdc.recipe_service.dev.domain.dto.recipebook;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
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

/** Dev V3 레시피북 내 레시피 항목 응답. */
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "Dev V3 레시피북 내 레시피 항목")
public class DevRecipeBookItemResponse {

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

    @Schema(description = "즐겨찾기 수")
    private Long favoriteCount;

    @Schema(description = "조리 시간(분)")
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

    @Schema(description = "유튜브에서 가져온 레시피 여부")
    private boolean isYoutube;

    @Schema(description = "AI가 생성한 레시피 여부")
    private boolean isAiGenerated;

    @Schema(description = "레시피 가시성", example = "PUBLIC")
    private String visibility;

    @Schema(description = "lifecycle 상태", example = "ACTIVE")
    private String lifecycleStatus;

    @Schema(description = "source", example = "USER")
    private String source;

    // 필드명 'remix' — Lombok isRemix() getter가 Jackson에 "remix"로 이중 등록되는 것 회피.
    @Schema(description = "remix(클론) 레시피 여부", example = "true")
    @JsonProperty("isRemix")
    private boolean remix;

    public static DevRecipeBookItemResponse from(RecipeBookItem item, String imageUrl) {
        Recipe recipe = item.getRecipe();
        User author = recipe.getUser();
        String youtubeUrl = recipe.getYoutubeUrl();
        boolean isYoutube = youtubeUrl != null && !youtubeUrl.isEmpty();

        return DevRecipeBookItemResponse.builder()
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
                // dev V3 4 enum
                .visibility(recipe.getVisibility() != null ? recipe.getVisibility().name() : null)
                .lifecycleStatus(recipe.getLifecycleStatus() != null ? recipe.getLifecycleStatus().name() : null)
                .source(recipe.getSource() != null ? recipe.getSource().name() : null)
                .remix(recipe.getOriginRecipe() != null)  // builder: 필드명 'remix' → setter '.remix(...)'
                .build();
    }
}
