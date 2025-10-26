package com.jdc.recipe_service.domain.dto.v2.recipe;

import lombok.Getter;
import lombok.Setter;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class RecipeStatusRequest {

    private List<Long> recipeIds;
}