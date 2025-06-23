package com.jdc.recipe_service.opensearch.dto;

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

    public FridgeRecipeDto(
            RecipeSimpleDto simple,
            List<String> matchedIngredients) {
        super(
                simple.getId(),
                simple.getTitle(),
                simple.getImageUrl(),
                simple.getAuthorId(),
                simple.getAuthorName(),
                simple.getProfileImage(),
                simple.getCreatedAt(),
                simple.getLikeCount(),
                simple.isLikedByCurrentUser(),
                simple.getCookingTime(),
                simple.getAvgRating(),
                simple.getRatingCount()
        );
        this.matchedIngredients = matchedIngredients;
    }
}