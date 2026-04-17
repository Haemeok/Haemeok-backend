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

    @Schema(description = "현재 사용자가 볼 수 있는 레시피 수 (공개 레시피 + 본인 레시피). "
            + "타인이 비공개로 전환한 레시피는 집계에서 제외된다.")
    private int recipeCount;

    public static RecipeBookResponse from(RecipeBook book, int recipeCount) {
        return RecipeBookResponse.builder()
                .id(book.getId())
                .name(book.getName())
                .isDefault(book.isDefault())
                .displayOrder(book.getDisplayOrder())
                .recipeCount(recipeCount)
                .build();
    }
}
