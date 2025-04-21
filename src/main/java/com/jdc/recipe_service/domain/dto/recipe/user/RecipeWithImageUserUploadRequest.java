package com.jdc.recipe_service.domain.dto.recipe.user;

import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

import java.util.List;

@Getter
@NoArgsConstructor
@AllArgsConstructor
public class RecipeWithImageUserUploadRequest {
    private RecipeUserCreateRequestDto recipe;
    private List<FileInfoRequest> files;
}
