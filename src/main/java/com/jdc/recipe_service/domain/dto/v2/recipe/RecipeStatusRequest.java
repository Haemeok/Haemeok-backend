package com.jdc.recipe_service.domain.dto.v2.recipe;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
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

    @JsonDeserialize(contentUsing = HashIdConfig.HashIdDeserializer.class)
    private List<Long> recipeIds;
}