package com.jdc.recipe_service.domain.dto.recipebook;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Size;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "레시피북에 레시피 추가 요청 (bulk)")
public class AddRecipesToBookRequest {

    @NotEmpty(message = "추가할 레시피 ID 목록은 필수입니다.")
    @Size(max = 50, message = "한 번에 최대 50개까지 추가할 수 있습니다.")
    @JsonDeserialize(contentUsing = HashIdConfig.HashIdDeserializer.class)
    @Schema(description = "추가할 레시피 ID 목록")
    private List<Long> recipeIds;
}
