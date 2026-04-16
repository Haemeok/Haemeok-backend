package com.jdc.recipe_service.domain.dto.recipebook;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "레시피북에 레시피 추가 요청")
public class AddRecipeToBookRequest {

    @NotNull(message = "레시피 ID는 필수입니다.")
    @JsonDeserialize(using = HashIdConfig.HashIdDeserializer.class)
    @Schema(description = "추가할 레시피 ID")
    private Long recipeId;
}
