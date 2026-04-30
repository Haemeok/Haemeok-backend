package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDtoV2;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * Dev V3 budget API 응답 DTO.
 *
 * 운영 {@link RecipeSimpleStaticDtoV2}의 모든 필드(cost/marketPrice 포함)를 @SuperBuilder로 상속하고
 * dev V3에서 도입한 4-enum (visibility / listingStatus / lifecycleStatus / source)을 추가 노출.
 *
 * popular는 cost/marketPrice가 필요 없어 {@link DevRecipeSimpleStaticDto} 사용. budget만 V2 변형.
 */
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@Schema(description = "Dev V3 budget 응답 DTO (V2 base + cost + 4 enum)")
public class DevRecipeSimpleStaticDtoV2 extends RecipeSimpleStaticDtoV2 {

    @Schema(description = "레시피 가시성", example = "PUBLIC")
    private String visibility;

    @Schema(description = "레시피 listing 상태", example = "LISTED")
    private String listingStatus;

    @Schema(description = "레시피 lifecycle 상태", example = "ACTIVE")
    private String lifecycleStatus;

    @Schema(description = "레시피 source", example = "USER")
    private String source;

    /**
     * JPQL projection 생성자 — dev budget query에서 사용.
     * 운영 V2 base의 첫 번째 (@QueryProjection) 생성자 시그니처와 일치 (long primitive + BigDecimal avgRating).
     * JPQL의 {@code COALESCE(r.avgRating, 0.0)}이 BigDecimal을 반환하므로 BigDecimal로 받아야 매칭됨.
     */
    public DevRecipeSimpleStaticDtoV2(
            Long id, String title, String imageUrl, Long authorId, String authorName, String profileImage,
            LocalDateTime createdAt, Integer cookingTime,
            long likeCount, Long favoriteCount, BigDecimal avgRating, long ratingCount,
            Integer ingredientCost, Integer marketPrice,
            String youtubeChannelName, String youtubeChannelId, String youtubeVideoTitle, String youtubeThumbnailUrl,
            String youtubeChannelProfileUrl, Long youtubeSubscriberCount, Long youtubeVideoViewCount,
            String youtubeUrl, Boolean isAiGenerated,
            RecipeVisibility visibility, RecipeListingStatus listingStatus,
            RecipeLifecycleStatus lifecycleStatus, RecipeSourceType source) {
        super(id, title, imageUrl, authorId, authorName, profileImage, createdAt, cookingTime,
                likeCount, favoriteCount, avgRating, ratingCount, ingredientCost, marketPrice,
                youtubeChannelName, youtubeChannelId, youtubeVideoTitle, youtubeThumbnailUrl,
                youtubeChannelProfileUrl, youtubeSubscriberCount, youtubeVideoViewCount,
                youtubeUrl, isAiGenerated);
        this.visibility = (visibility != null) ? visibility.name() : null;
        this.listingStatus = (listingStatus != null) ? listingStatus.name() : null;
        this.lifecycleStatus = (lifecycleStatus != null) ? lifecycleStatus.name() : null;
        this.source = (source != null) ? source.name() : null;
    }
}
