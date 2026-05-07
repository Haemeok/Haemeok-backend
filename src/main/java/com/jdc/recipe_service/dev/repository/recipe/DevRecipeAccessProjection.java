package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;

/**
 * Dev V3 batch 접근/표시 가능 여부 판단용 projection.
 * Recipe entity 전체 로드 없이 N개 ID 일괄 조회 — N+1 lazy load 회피.
 */
public record DevRecipeAccessProjection(
        Long recipeId,
        Long ownerId,
        RecipeLifecycleStatus lifecycleStatus,
        RecipeVisibility visibility,
        RecipeListingStatus listingStatus,
        RecipeImageStatus imageStatus,
        Long originRecipeId
) {
    /** dev imageReady 컨벤션 — null 또는 READY만 표시 가능. */
    public boolean isImageReady() {
        return imageStatus == null || imageStatus == RecipeImageStatus.READY;
    }

    public boolean isRemix() {
        return originRecipeId != null;
    }
}
