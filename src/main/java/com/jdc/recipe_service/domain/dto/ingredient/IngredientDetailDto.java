package com.jdc.recipe_service.domain.dto.ingredient;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "재료 상세 (i버튼 팝업용): 보관법과 해당 재료로 만들 수 있는 인기 레시피 목록")
public class IngredientDetailDto {

    @Schema(description = "재료 ID (해시)", example = "xJvY7aBp")
    @JsonSerialize(using = HashIdSerializer.class)
    private Long id;

    @Schema(description = "재료명", example = "대파")
    private String name;

    @Schema(description = "재료 카테고리", example = "채소")
    private String category;

    @Schema(description = "재료 이미지 URL",
            example = "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/ingredients/대파.webp")
    private String imageUrl;

    @Schema(description = "보관 방법 (자유 텍스트). 없으면 null.",
            example = "냉장 보관 권장", nullable = true)
    private String storageMethod;

    @Schema(description = "이 재료로 만들 수 있는 공개 레시피 (popularityScore 내림차순, 최대 10개). 없으면 빈 배열.")
    private List<RecipeSimpleDto> recipes;
}
