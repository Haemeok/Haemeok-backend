package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
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

/**
 * Dev V3 search 응답 DTO.
 *
 * 운영 {@link RecipeSimpleStaticDto}의 모든 필드를 그대로 상속(@SuperBuilder)하고, dev V3에서 새로 도입한
 * 4-enum을 추가 노출한다. 프론트가 RESTRICTED 활성화 후 UI 분기(예: "비공개 표시" 뱃지)할 때 사용.
 *
 * 운영 응답 contract는 손대지 않으므로 V2 path의 {@link RecipeSimpleStaticDto}는 변경 없음.
 */
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@Schema(description = "Dev V3 레시피 검색 결과 DTO (V2 base + 4 enum 추가)")
public class DevRecipeSimpleStaticDto extends RecipeSimpleStaticDto {

    @Schema(description = "레시피 가시성", example = "PUBLIC")
    private String visibility;

    @Schema(description = "레시피 listing 상태", example = "LISTED")
    private String listingStatus;

    @Schema(description = "레시피 lifecycle 상태", example = "ACTIVE")
    private String lifecycleStatus;

    @Schema(description = "레시피 source", example = "USER")
    private String source;

    /**
     * QueryDSL projection용 생성자.
     * QRecipe의 EnumPath는 enum 인스턴스를 반환하므로 enum 타입으로 받아 name()으로 직렬화.
     */
    @QueryProjection
    public DevRecipeSimpleStaticDto(
            Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
            LocalDateTime createdAt, Integer cookingTime, long likeCount, Long favoriteCount, BigDecimal avgRating, long ratingCount,
            String youtubeChannelName, String youtubeChannelId, String youtubeVideoTitle, String youtubeThumbnailUrl,
            String youtubeChannelProfileUrl, Long youtubeSubscriberCount, Long youtubeVideoViewCount,
            String youtubeUrl, Boolean isAiGenerated,
            RecipeVisibility visibility, RecipeListingStatus listingStatus,
            RecipeLifecycleStatus lifecycleStatus, RecipeSourceType source) {
        super(id, title, imageUrl, authorId, authorName, profileImage, createdAt, cookingTime,
                likeCount, favoriteCount, avgRating, ratingCount,
                youtubeChannelName, youtubeChannelId, youtubeVideoTitle, youtubeThumbnailUrl,
                youtubeChannelProfileUrl, youtubeSubscriberCount, youtubeVideoViewCount,
                youtubeUrl, isAiGenerated);
        this.visibility = (visibility != null) ? visibility.name() : null;
        this.listingStatus = (listingStatus != null) ? listingStatus.name() : null;
        this.lifecycleStatus = (lifecycleStatus != null) ? lifecycleStatus.name() : null;
        this.source = (source != null) ? source.name() : null;
    }
}
