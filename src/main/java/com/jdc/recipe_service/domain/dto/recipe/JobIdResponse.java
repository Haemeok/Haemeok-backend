package com.jdc.recipe_service.domain.dto.recipe;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig;
import io.swagger.v3.oas.annotations.media.Schema;

@Schema(description = "비동기 작업 ID 응답")
public record JobIdResponse(
        @Schema(description = "생성된 작업 ID (암호화됨)", example = "Xy7zK9")
        @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
        Long jobId
) {}