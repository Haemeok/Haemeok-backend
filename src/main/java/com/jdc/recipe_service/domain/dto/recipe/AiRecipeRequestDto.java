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

    private List<String> tagNames;
    private Integer spiceLevel;
    private String allergy;
}