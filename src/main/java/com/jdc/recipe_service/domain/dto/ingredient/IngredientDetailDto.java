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

    @Schema(description = "보관 위치 (실온/냉장/냉동). 없으면 null.",
            example = "냉장", nullable = true)
    private String storageLocation;

    @Schema(description = "보관 온도 상세. 없으면 null.",
            example = "0~4℃", nullable = true)
    private String storageTemperature;

    @Schema(description = "보관 기간. 없으면 null.",
            example = "냉장 1~2일 / 냉동 2~3개월", nullable = true)
    private String storageDuration;

    @Schema(description = "보관 참고사항. 없으면 null.",
            example = "구입 당일 섭취 권장. 손질 후 밀봉하여 냉동", nullable = true)
    private String storageNotes;

    @Schema(description = "잘 어울리는 재료 (슬래시 구분). 없으면 null.",
            example = "버터 / 마늘 / 레몬", nullable = true)
    private String goodPairs;

    @Schema(description = "상극 재료 (슬래시 구분). 없으면 null.",
            example = "우유 / 식초", nullable = true)
    private String badPairs;

    @Schema(description = "추천 조리법 (슬래시 구분). 없으면 null.",
            example = "찜 / 버터구이 / 오븐구이", nullable = true)
    private String recommendedCookingMethods;

    @Schema(description = "이 재료로 만들 수 있는 공개 레시피 (popularityScore 내림차순, 최대 10개). 없으면 빈 배열.")
    private List<RecipeSimpleDto> recipes;
}
