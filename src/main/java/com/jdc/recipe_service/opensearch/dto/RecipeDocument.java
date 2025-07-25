package com.jdc.recipe_service.opensearch.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
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
    private int likeCount;
    private int cookingTime;
    private String imageUrl;
    private Boolean  isAiGenerated;
    private List<Long> ingredientIds;
    private Integer ingredientCount;
    private BigDecimal avgRating;
}
