package com.jdc.recipe_service.domain.dto;

import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import com.jdc.recipe_service.opensearch.dto.AiRecipeFilter;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class RecipeSearchCondition {
    private String title;
    private String dishType;
    private List<String> tags;
    private AiRecipeFilter aiFilter;

    @Schema(description = "최소 재료비 (원)", example = "0")
    private Integer minCost;
    @Schema(description = "최대 재료비 (원)", example = "10000")
    private Integer maxCost;

    @Schema(description = "최소 칼로리 (kcal)", example = "0")
    private Integer minCalories;
    @Schema(description = "최대 칼로리 (kcal)", example = "500")
    private Integer maxCalories;

    @Schema(description = "최소 단백질 (g)", example = "0")
    private Integer minProtein;
    @Schema(description = "최대 단백질 (g)", example = "20")
    private Integer maxProtein;

    @Schema(description = "최소 탄수화물 (g)", example = "0")
    private Integer minCarb;
    @Schema(description = "최대 탄수화물 (g)", example = "50")
    private Integer maxCarb;

    @Schema(description = "최소 지방 (g)", example = "0")
    private Integer minFat;
    @Schema(description = "최대 지방 (g)", example = "15")
    private Integer maxFat;

    @Schema(description = "최소 당류 (g)", example = "0")
    private Integer minSugar;
    @Schema(description = "최대 당류 (g)", example = "10")
    private Integer maxSugar;

    @Schema(description = "최소 나트륨 (mg)", example = "0")
    private Integer minSodium;
    @Schema(description = "최대 나트륨 (mg)", example = "1000")
    private Integer maxSodium;

    public DishType getDishTypeEnum() {
        if (dishType == null || dishType.isBlank()) return null;
        return DishType.fromCode(dishType);
    }

    public List<TagType> getTagEnums() {
        if (tags == null || tags.isEmpty()) return List.of();
        return tags.stream()
                .map(TagType::fromCode)
                .toList();
    }

    public AiRecipeFilter getAiFilter() {
        return this.aiFilter == null ? AiRecipeFilter.USER_ONLY : this.aiFilter;
    }

    public void setQ(String q) {
        this.title = q;
    }
}