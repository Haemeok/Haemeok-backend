package com.jdc.recipe_service.domain.dto.recipe;

import lombok.*;

@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AiImageTestRequestDto {
    private RecipeCreateRequestDto requestData;
    private String prompt;
}