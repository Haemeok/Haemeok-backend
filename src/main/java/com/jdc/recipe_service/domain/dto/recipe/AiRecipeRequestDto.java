package com.jdc.recipe_service.domain.dto.recipe;

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
    private int cookingTime;
}