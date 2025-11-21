package com.jdc.recipe_service.domain.dto.ai;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@NoArgsConstructor
public class RecipeAnalysisResponseDto {

    private String cookingTips;
    private Integer marketPrice;
    private boolean isAbusive;
}