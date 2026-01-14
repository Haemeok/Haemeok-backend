package com.jdc.recipe_service.domain.dto.v2.recipe;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import com.jdc.recipe_service.domain.dto.recipe.RecipeNutritionDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepDto;
import com.jdc.recipe_service.domain.dto.user.UserDto;
import com.jdc.recipe_service.domain.dto.v2.comment.CommentStaticDto;
import com.jdc.recipe_service.domain.dto.v2.rating.RecipeRatingInfoStaticDto;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

/**
 *
 *  레시피 상세 조회용
 */

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class RecipeDetailStaticDto {

    @Schema(description = "레시피 ID")
    @JsonSerialize(using = HashIdSerializer.class)
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

    @Schema(description = "유튜브 링트 URL")
    private String youtubeUrl;

    @Schema(description = "유튜브 채널명")
    private String youtubeChannelName;

    @Schema(description = "유튜브 원본 영상 제목")
    private String youtubeVideoTitle;

    @Schema(description = "유튜브 썸네일 URL")
    private String youtubeThumbnailUrl;

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

    @Schema(description = "평점 정보")
    private RecipeRatingInfoStaticDto ratingInfo;

    @Schema(description = "태그 목록")
    private List<String> tags;

    @Schema(description = "재료 목록")
    private List<RecipeIngredientDto> ingredients;

    @Schema(description = "조리 단계 목록")
    private List<RecipeStepDto> steps;

    @Schema(description = "댓글 목록")
    private List<CommentStaticDto> comments;

    @Schema(description = "전체 댓글 수")
    private long commentCount;

    @Schema(description = "총 재료 비용")
    private Integer totalIngredientCost;

    @Schema(description = "총 칼로리")
    private Double totalCalories;

    @Schema(description = "예상 마켓 가격")
    private Integer marketPrice;

    @Schema(description = "절약 금액")
    private Integer savings;

    @Schema(description = "요리 팁")
    private String cookingTips;

    @Schema(description = "레시피 총 영양성분")
    private RecipeNutritionDto nutrition;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
    @Schema(description = "생성일시 (UTC)")
    private LocalDateTime createdAt;

    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss", timezone = "Asia/Seoul")
    @Schema(description = "업데이트일시 (UTC)")
    private LocalDateTime updatedAt;

    @Schema(description = "파인다이닝 전용 상세 정보 (일반 레시피일 경우 null)")
    private FineDiningInfo fineDiningInfo;

    @Getter @Builder @NoArgsConstructor @AllArgsConstructor
    public static class FineDiningInfo {
        @Schema(description = "컴포넌트 목록")
        private List<FineDiningComponentDto> components;
        @Schema(description = "플레이팅 가이드 정보")
        private FineDiningPlatingDto plating;
    }

    @Getter @Builder @NoArgsConstructor @AllArgsConstructor
    public static class FineDiningComponentDto {
        private String role;
        private String name;
        private String description;
    }

    @Getter @Builder @NoArgsConstructor @AllArgsConstructor
    public static class FineDiningPlatingDto {
        private String vessel;
        private String guide;
    }
}
