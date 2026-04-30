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
@Builder(toBuilder = true)
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

    @Schema(description = "Coupang Partners link for this ingredient. Returns null when no link is available.",
            example = "https://link.coupang.com/a/example", nullable = true)
    private String coupangLink;

    @Schema(description = "100g당 영양성분. 마스터 per-g 데이터가 없으면 각 값은 null일 수 있습니다.", nullable = true)
    private IngredientNutritionPer100gDto nutritionPer100g;

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

    @Schema(description = "goodPairs를 파싱한 구조화 목록. DB에 존재하는 재료는 id/imageUrl을 포함하고, 자유 텍스트는 id/imageUrl이 null입니다.")
    private List<IngredientPairItemDto> goodPairItems;

    @Schema(description = "상극 재료 (슬래시 구분). 없으면 null.",
            example = "우유 / 식초", nullable = true)
    private String badPairs;

    @Schema(description = "badPairs를 파싱한 구조화 목록. DB에 존재하는 재료는 id/imageUrl을 포함하고, 자유 텍스트는 id/imageUrl이 null입니다.")
    private List<IngredientPairItemDto> badPairItems;

    @Schema(description = "재료 효능 설명. 데이터가 없으면 null.", example = "비타민 C와 식이섬유가 풍부합니다.", nullable = true)
    private String benefits;

    @Schema(description = "제철 월 목록(1~12). 데이터가 없으면 null.", example = "[6,7,8]", nullable = true)
    private List<Integer> seasonMonths;

    @Schema(description = "추천 조리법 (슬래시 구분). 없으면 null.",
            example = "찜 / 버터구이 / 오븐구이", nullable = true)
    private String recommendedCookingMethods;

    @Schema(description = "이 재료로 만들 수 있는 공개 레시피 (popularityScore 내림차순, 최대 10개). 없으면 빈 배열.")
    private List<RecipeSimpleDto> recipes;
}
