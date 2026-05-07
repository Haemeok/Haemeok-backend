package com.jdc.recipe_service.dev.domain.dto.recipe;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

/** Dev V3 favorites/사용자 동적 응답 DTO. */
@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@Schema(description = "Dev V3 레시피 간략 DTO")
public class DevRecipeSimpleDto extends RecipeSimpleDto {

    @Schema(description = "레시피 가시성", example = "PUBLIC")
    private String visibility;

    @Schema(description = "lifecycle 상태", example = "ACTIVE")
    private String lifecycleStatus;

    @Schema(description = "source", example = "USER")
    private String source;

    // 필드명 'remix' — Lombok isRemix() getter가 Jackson에 "remix"로 이중 등록되는 것 회피.
    @Schema(description = "remix(클론) 레시피 여부", example = "true")
    @JsonProperty("isRemix")
    private boolean remix;
}
