package com.jdc.recipe_service.domain.dto.recipe;

import com.jdc.recipe_service.domain.type.JobStatus;
import lombok.Builder;
import lombok.Getter;

@Getter
@Builder
public class JobStatusDto {
    private Long jobId;
    private JobStatus status;
    private Long resultRecipeId;
    private String errorMessage;
    private int progress;
}