package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;

/**
 * Dev V3 batch 접근/표시 가능 여부 판단용 projection.
 *
 * Recipe entity 전체를 로드하지 않고 4-enum + ownerId + imageStatus만 가져와서 후속 가시성/표시 필터에 직접 넣는다.
 * batch status / batch saved-books / recommendations post-filter 등 N개 ID 입력에서 N+1 lazy load 회피.
 *
 * <p>필드 사용 분기:
 *  - {@code recipeId}, {@code ownerId}, lifecycle/visibility/listing → {@link
 *    com.jdc.recipe_service.dev.policy.recipe.DevRecipeAccessPolicy#isAccessibleBy} (모든 consumer)
 *  - {@code imageStatus} → recommendations 카드 표시 가능 여부 (READY OR NULL — dev imageReady 컨벤션). status batch는 사용 안 함.
 */
public record DevRecipeAccessProjection(
        Long recipeId,
        Long ownerId,
        RecipeLifecycleStatus lifecycleStatus,
        RecipeVisibility visibility,
        RecipeListingStatus listingStatus,
        RecipeImageStatus imageStatus
) {
    /** dev imageReady 컨벤션 — 다른 dev 경로 (favorites, fridge, user lists, recipe books, remixes) 와 정합. */
    public boolean isImageReady() {
        return imageStatus == null || imageStatus == RecipeImageStatus.READY;
    }
}
