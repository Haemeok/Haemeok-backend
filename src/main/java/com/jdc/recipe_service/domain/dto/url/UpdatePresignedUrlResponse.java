package com.jdc.recipe_service.domain.dto.url;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UpdatePresignedUrlResponse {
    private List<PresignedUrlResponseItem> uploads;
}
