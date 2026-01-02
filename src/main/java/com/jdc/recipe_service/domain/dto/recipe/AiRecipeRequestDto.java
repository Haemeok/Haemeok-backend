package com.jdc.recipe_service.domain.dto.recipe;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.type.DiningTier;
import lombok.*;

import java.util.List;

@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AiRecipeRequestDto {
    @JsonDeserialize(using = HashIdConfig.HashIdDeserializer.class)
    private Long userId;
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

    @JsonDeserialize(contentUsing = HashIdConfig.HashIdDeserializer.class)
    private List<Long> ingredientIds;
    private DiningTier diningTier;
}