package com.jdc.recipe_service.domain.dto.ai;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter @Setter
@NoArgsConstructor
public class RecipeAnalysisResponseDto {

    private String cookingTips;
    private Integer marketPrice;
    @JsonProperty("isAbusive")
    private boolean isAbusive;
    private String abuseType;
}