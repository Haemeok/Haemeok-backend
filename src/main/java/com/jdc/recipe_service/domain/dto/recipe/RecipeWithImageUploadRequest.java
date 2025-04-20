package com.jdc.recipe_service.domain.dto.recipe;

import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@NoArgsConstructor
@AllArgsConstructor
public class RecipeWithImageUploadRequest {
    private RecipeCreateRequestDto recipe;
    private List<FileInfoRequest> files;
}