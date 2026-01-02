package com.jdc.recipe_service.domain.dto.url;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.AllArgsConstructor;
import lombok.Getter;

import java.util.List;

@Getter
@AllArgsConstructor
public class FinalizeResponse {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long recipeId;
    private List<String> activeImages;
    private List<String> missingImages;
}
