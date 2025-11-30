package com.jdc.recipe_service.domain.dto.recipe;

import lombok.*;

@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AiPromptRequestDto {
    private String prompt;
    private AiRecipeRequestDto requestData;
}