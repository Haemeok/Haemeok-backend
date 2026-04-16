package com.jdc.recipe_service.domain.dto.recipebook;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import com.jdc.recipe_service.domain.entity.RecipeBook;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "레시피북 응답")
public class RecipeBookResponse {

    @JsonSerialize(using = HashIdSerializer.class)
    @Schema(description = "레시피북 ID")
    private Long id;

    @Schema(description = "레시피북 이름", example = "한식 모음")
    private String name;

    @Schema(description = "기본 레시피북 여부")
    private boolean isDefault;

    @Schema(description = "표시 순서")
    private int displayOrder;

    @Schema(description = "포함된 레시피 수")
    private int recipeCount;

    public static RecipeBookResponse from(RecipeBook book) {
        return RecipeBookResponse.builder()
                .id(book.getId())
                .name(book.getName())
                .isDefault(book.isDefault())
                .displayOrder(book.getDisplayOrder())
                .recipeCount(book.getRecipeCount())
                .build();
    }
}
