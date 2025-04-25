package com.jdc.recipe_service.domain.dto.url;

import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter
@AllArgsConstructor
public class FinalizeResponse {
    private Long recipeId;
    private List<String> activeImages;
    private List<String> missingImages;
}
