package com.jdc.recipe_service.domain.dto.recipebook;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;

@Getter
@AllArgsConstructor
@Builder
@Schema(description = "레시피북 레시피 추가 결과")
public class AddRecipesToBookResponse {

    @Schema(description = "실제로 추가된 레시피 수", example = "3")
    private int addedCount;

    @Schema(description = "이미 존재하거나 접근 불가하여 건너뛴 레시피 수", example = "1")
    private int skippedCount;
}
