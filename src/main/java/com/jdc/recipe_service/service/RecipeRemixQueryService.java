package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class RecipeRemixQueryService {

    private final RecipeRepository recipeRepository;

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> findRemixes(Long originRecipeId, Pageable pageable) {
        if (!recipeRepository.existsById(originRecipeId)) {
            throw new CustomException(ErrorCode.RECIPE_NOT_FOUND);
        }
        return recipeRepository.findRemixesByOriginRecipeId(originRecipeId, pageable);
    }
}
