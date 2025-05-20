package com.jdc.recipe_service.domain.dto.recipe;

import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import lombok.*;

import java.util.List;

@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RecipeWithImageUploadRequest {
    private RecipeCreateRequestDto recipe;
    private List<FileInfoRequest> files;

    private AiRecipeRequestDto aiRequest;
}