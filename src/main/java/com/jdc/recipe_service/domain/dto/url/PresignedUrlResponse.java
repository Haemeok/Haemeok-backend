package com.jdc.recipe_service.domain.dto.url;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig.HashIdSerializer;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PresignedUrlResponse {
    @JsonSerialize(using = HashIdSerializer.class)
    private Long recipeId;
    private List<PresignedUrlResponseItem> uploads;

    @JsonIgnore
    @Builder.Default
    private boolean created = false;
}