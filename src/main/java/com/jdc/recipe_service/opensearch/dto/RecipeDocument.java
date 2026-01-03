package com.jdc.recipe_service.opensearch.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RecipeDocument {
    private Long id;
    private String title;
    private List<String> tags;
    private String dishType;
    private String createdAt;
    private int cookingTime;
    private String imageUrl;
    private String youtubeUrl;
    private Boolean  isAiGenerated;
    private Boolean  isPrivate;
    private List<Long> ingredientIds;
    private List<String> ingredientNames;
    private Integer ingredientCount;

    private Integer totalIngredientCost;
    private Float totalCalories;
    private Float protein;
    private Float carbohydrate;
    private Float fat;
    private Float sugar;
    private Float sodium;
}
