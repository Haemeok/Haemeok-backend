package com.jdc.recipe_service.domain.dto.recipe;

import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description = "비동기 작업 ID 응답")
public record JobIdResponse(
        @Schema(description = "생성된 작업 ID", example = "123")
        Long jobId
) {}