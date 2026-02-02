package com.jdc.recipe_service.domain.dto.recipe;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.jdc.recipe_service.config.HashIdConfig;
import com.jdc.recipe_service.domain.type.JobStatus;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class JobStatusDto {
    @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
    private Long jobId;
    private JobStatus status;

    @JsonSerialize(using = HashIdConfig.HashIdSerializer.class)
    private Long resultRecipeId;

    private String errorCode;
    private String errorMessage;
    private int progress;
}