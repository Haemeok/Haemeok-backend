package com.jdc.recipe_service.domain.dto.url;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class PresignedUrlResponse {
    private List<PresignedUrlResponseItem> uploads;
}