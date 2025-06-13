package com.jdc.recipe_service.domain.dto.recipe;

import com.jdc.recipe_service.domain.type.SaltinessPreference;
import lombok.*;

import java.util.List;

@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AiRecipeRequestDto {
    private List<String> ingredients;
    private List<String> tagNames;
    private String dishType;
    private Integer cookingTime;
    private Double servings;

    private Integer spiceLevel;
    private SaltinessPreference saltiness;
    private String allergy;
    private String dietType;
}