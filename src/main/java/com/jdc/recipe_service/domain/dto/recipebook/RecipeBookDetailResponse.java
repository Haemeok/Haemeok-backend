package com.jdc.recipe_service.domain.dto.recipebook;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "레시피북 상세 응답 (레시피 목록 포함)")
public class RecipeBookDetailResponse {

    @JsonSerialize(using = HashIdSerializer.class)
    @Schema(description = "레시피북 ID")
    private Long id;

    @Schema(description = "레시피북 이름")
    private String name;

    @Schema(description = "기본 레시피북 여부")
    private boolean isDefault;

    @Schema(description = "포함된 레시피 수")
    private int recipeCount;

    @Schema(description = "레시피 목록 (최신순)")
    private List<RecipeBookItemResponse> recipes;

    @Schema(description = "다음 페이지 존재 여부")
    private boolean hasNext;
}
