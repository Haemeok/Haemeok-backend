package com.jdc.recipe_service.domain.dto.recipebook;

import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "레시피북 생성 요청")
public class CreateRecipeBookRequest {

    @NotBlank(message = "레시피북 이름은 필수입니다.")
    @Size(max = 50, message = "레시피북 이름은 50자 이내여야 합니다.")
    @Schema(description = "레시피북 이름", example = "한식 모음")
    private String name;
}
