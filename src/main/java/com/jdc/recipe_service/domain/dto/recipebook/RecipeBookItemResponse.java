package com.jdc.recipe_service.domain.dto.recipebook;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeBookItem;
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

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
    @Schema(description = "레시피북에 추가된 시간")
    private LocalDateTime addedAt;

    public static RecipeBookItemResponse from(RecipeBookItem item, String imageUrl) {
        Recipe recipe = item.getRecipe();
        return RecipeBookItemResponse.builder()
                .recipeId(recipe.getId())
                .title(recipe.getTitle())
                .imageUrl(imageUrl)
                .dishType(recipe.getDishType() != null ? recipe.getDishType().name() : null)
                .addedAt(item.getCreatedAt())
                .build();
    }
}
