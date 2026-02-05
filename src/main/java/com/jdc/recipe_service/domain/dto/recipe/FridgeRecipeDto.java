package com.jdc.recipe_service.domain.dto.recipe;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.*;
import lombok.experimental.SuperBuilder;

import java.util.List;

@EqualsAndHashCode(callSuper = true)
@Getter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@Schema(description = "냉장고 기반 레시피 DTO (RecipeSimpleDto + 겹치는 재료)")
public class FridgeRecipeDto extends RecipeSimpleDto {

    @Schema(description = "내 냉장고 재료와 겹치는 재료 이름 리스트")
    private List<String> matchedIngredients;

    @Schema(description = "부족한 재료 이름과 구매 링크 리스트")
    private List<MissingIngredientDto> missingIngredients;

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class MissingIngredientDto {
        private String name;
        private String coupangLink;
    }

    public FridgeRecipeDto(
            RecipeSimpleDto simple,
            List<String> matchedIngredients,
            List<MissingIngredientDto> missingIngredients) {
        super(
                simple.getId(),
                simple.getTitle(),
                simple.getImageUrl(),
                simple.getAuthorId(),
                simple.getAuthorName(),
                simple.getProfileImage(),
                simple.getCreatedAt(),
                simple.getFavoriteCount(),
                simple.getLikeCount(),
                simple.isLikedByCurrentUser(),
                simple.getCookingTime(),
                simple.getYoutubeChannelName(),
                simple.getYoutubeChannelId(),
                simple.getYoutubeVideoTitle(),
                simple.getYoutubeThumbnailUrl(),
                simple.getYoutubeChannelProfileUrl(),
                simple.getYoutubeSubscriberCount(),
                simple.getYoutubeVideoViewCount(),
                simple.getAvgRating(),
                simple.getRatingCount(),
                null,
                simple.isAiGenerated()
        );
        this.setYoutube(simple.isYoutube());
        this.matchedIngredients = matchedIngredients;
        this.missingIngredients = missingIngredients;
    }
}
