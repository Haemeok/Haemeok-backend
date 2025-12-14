package com.jdc.recipe_service.domain.dto.recipe;

import lombok.*;

import java.util.List;

@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AiRecipeRequestDto {
    private Long userId;

    private List<String> ingredients;
    private String dishType;
    private Integer cookingTime;
    private Double servings;

    private List<String> tags;
    private Integer spiceLevel;
    private String allergy;

    private Integer targetBudget;
    private String targetCategory;

    private String targetCalories;
    private String targetCarbs;
    private String targetProtein;
    private String targetFat;
    private String targetStyle;
}