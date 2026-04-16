package com.jdc.recipe_service.domain.dto.recipebook;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.jdc.recipe_service.config.HashIdConfig;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Schema(description = "레시피북 순서 변경 요청")
public class ReorderRecipeBooksRequest {

    @NotEmpty(message = "정렬할 레시피북 ID 목록은 필수입니다.")
    @JsonDeserialize(contentUsing = HashIdConfig.HashIdDeserializer.class)
    @Schema(description = "정렬된 레시피북 ID 목록 (순서대로)", example = "[\"abc123\", \"def456\"]")
    private List<Long> bookIds;
}
