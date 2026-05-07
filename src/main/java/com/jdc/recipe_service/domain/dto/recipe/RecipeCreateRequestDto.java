package com.jdc.recipe_service.domain.dto.recipe;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import lombok.*;

import java.util.List;

/**
 * 레시피 생성용
 */

@Getter @Setter
@Builder(toBuilder = true)
@AllArgsConstructor
@NoArgsConstructor
public class RecipeCreateRequestDto {

    private Boolean isRecipe;
    private String nonRecipeReason;

    @NotBlank(message = "레시피 제목은 필수입니다.")
    private String title;
    private String description;
    private String cookingTips;
    @NotBlank(message = "요리 유형은 필수입니다.")
    private String dishType;

    private Integer cookingTime;
    private String imageKey;
    private RecipeImageStatus imageStatus;

    private RecipeVisibility visibility;
    private RecipeListingStatus listingStatus;
    private RecipeLifecycleStatus lifecycleStatus;

    private String youtubeUrl;
    private String youtubeChannelName;
    private String youtubeChannelId;
    private String youtubeVideoTitle;
    private String youtubeThumbnailUrl;
    private String youtubeChannelProfileUrl;
    private Long youtubeSubscriberCount;
    private Long youtubeVideoViewCount;
    private Long extractorId;

    private List<String> cookingTools;
    private Integer servings;
    private Integer totalIngredientCost;
    private Integer marketPrice;
    private Boolean isPrivate;
    private Double totalCalories;

    private RecipeNutritionDto nutrition;

    private List<RecipeIngredientRequestDto> ingredients;
    private List<RecipeStepRequestDto> steps;
    private List<String> tags;

    private List<ComponentResponseDto> components;
    private PlatingResponseDto plating;
    private List<String> imageMatchKeywords;

    @Schema(description = "리믹스(복제) 원본 레시피 ID. 지정 시 서버가 원본 cloneable 검증 후 " +
            "visibility=PUBLIC, listingStatus=UNLISTED, isPrivate=false (link-only), counts=0, source=YOUTUBE 으로 강제한다. " +
            "리믹스는 누구나 링크로 접근/저장/상호작용 가능하지만 검색/추천 등 discovery에는 노출되지 않는다.",
            example = "Xk8B2a9m")
    @JsonDeserialize(using = HashIdConfig.HashIdDeserializer.class)
    private Long originRecipeId;

    @Getter @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ComponentResponseDto {
        private String role;
        private String name;
        private String description;
        private List<String> process;
    }

    @Getter @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PlatingResponseDto {
        private String vessel;
        private String guide;
        private List<String> visualKeys;
        private String viewpoint;
        private String lighting;
    }
}
