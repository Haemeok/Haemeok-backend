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
public class PresignedUrlRequest {
    private Long recipeId;
    private List<FileInfoRequest> files;
}