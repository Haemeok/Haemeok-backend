package com.jdc.recipe_service.domain.dto;

import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.TagType;
import com.jdc.recipe_service.opensearch.dto.AiRecipeFilter;
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

    private Integer maxCost;
    private Integer maxCalories;
    private Integer maxProtein;
    private Integer maxCarb;
    private Integer maxFat;
    private Integer maxSugar;
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