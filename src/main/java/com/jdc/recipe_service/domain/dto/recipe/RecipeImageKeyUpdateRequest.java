package com.jdc.recipe_service.domain.dto.recipe;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@NoArgsConstructor
@AllArgsConstructor
public class RecipeImageKeyUpdateRequest {
    private String imageKey;
    private List<String> stepImageKeys;
}