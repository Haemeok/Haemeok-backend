package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.querydsl.core.annotations.QueryProjection;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/** Dev V3 search 응답 DTO. */
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@Schema(description = "Dev V3 레시피 검색 결과 DTO")
public class DevRecipeSimpleStaticDto extends RecipeSimpleStaticDto {

    @Schema(description = "레시피 가시성", example = "PUBLIC")
    private String visibility;

    @Schema(description = "레시피 lifecycle 상태", example = "ACTIVE")
    private String lifecycleStatus;

    @Schema(description = "레시피 source", example = "USER")
    private String source;

    // QRecipe EnumPath는 enum 인스턴스를 반환하므로 enum 타입으로 받아 name() 직렬화.
    @QueryProjection
    public DevRecipeSimpleStaticDto(
            Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
            LocalDateTime createdAt, Integer cookingTime, long likeCount, Long favoriteCount, BigDecimal avgRating, long ratingCount,
            String youtubeChannelName, String youtubeChannelId, String youtubeVideoTitle, String youtubeThumbnailUrl,
            String youtubeChannelProfileUrl, Long youtubeSubscriberCount, Long youtubeVideoViewCount,
            String youtubeUrl, Boolean isAiGenerated,
            RecipeVisibility visibility,
            RecipeLifecycleStatus lifecycleStatus, RecipeSourceType source) {
        super(id, title, imageUrl, authorId, authorName, profileImage, createdAt, cookingTime,
                likeCount, favoriteCount, avgRating, ratingCount,
                youtubeChannelName, youtubeChannelId, youtubeVideoTitle, youtubeThumbnailUrl,
                youtubeChannelProfileUrl, youtubeSubscriberCount, youtubeVideoViewCount,
                youtubeUrl, isAiGenerated);
        this.visibility = (visibility != null) ? visibility.name() : null;
        this.lifecycleStatus = (lifecycleStatus != null) ? lifecycleStatus.name() : null;
        this.source = (source != null) ? source.name() : null;
    }
}
