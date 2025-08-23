package com.jdc.recipe_service.domain.dto.recipe.v2;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import java.time.LocalDateTime;
import java.util.List;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeDetailStaticDto {

    @Schema(description = "레시피 ID")
    private Long id;

    @Schema(description = "레시피 제목")
    private String title;

    @Schema(description = "요리 유형 (dishType)")
    private String dishType;

    @Schema(description = "레시피 설명")
    private String description;

    @Schema(description = "예상 조리 시간 (분 단위)")
    private Integer cookingTime;

    @Schema(description = "대표 이미지 URL")
    private String imageUrl;

    @Schema(description = "대표 이미지 Key")
    private String imageKey;

    @Schema(description = "대표 이미지 상태 정보( PENDING, ACTIVE)")
    private String imageStatus;

    @Schema(description = "유튜브 링크 URL")
    private String youtubeUrl;

    @Schema(description = "조리 도구 목록")
    private List<String> cookingTools;

    @Schema(description = "AI 생성 여부")
    private boolean isAiGenerated;

    @Schema(description = "공개 여부")
    private boolean isPrivate;

    @Schema(description = "레시피 인분 수")
    private Integer servings;

    @Schema(description = "작성자 정보")
    private UserDto author;

    @Schema(description = "태그 목록")
    private List<String> tags;

    @Schema(description = "재료 목록")
    private List<RecipeIngredientDto> ingredients;

    @Schema(description = "조리 단계 목록")
    private List<RecipeStepDto> steps;

    @Schema(description = "총 재료 비용")
    private Integer totalIngredientCost;

    @Schema(description = "총 칼로리")
    private Double totalCalories;

    @Schema(description = "예상 마켓 가격")
    private Integer marketPrice;

    @Schema(description = "절약 금액")
    private Integer savings;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    @Schema(description = "생성일시 (UTC)")
    private LocalDateTime createdAt;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    @Schema(description = "업데이트일시 (UTC)")
    private LocalDateTime updatedAt;
}