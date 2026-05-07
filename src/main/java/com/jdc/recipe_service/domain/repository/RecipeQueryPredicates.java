package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.querydsl.core.types.dsl.BooleanExpression;

/**
 * 운영(non-dev) 검색/추천/리스팅 쿼리의 discovery 필터 헬퍼.
 *
 * <p>이전엔 {@code recipe.isPrivate.isFalse()} 한 줄로만 가시성을 판단했는데, V1.x 정책에서
 * PUBLIC+UNLISTED(link-only)는 isPrivate=false라도 검색/추천에 노출되면 안 된다.
 * 모든 discovery 쿼리는 4-enum 단일 정책 — {@code lifecycleStatus = ACTIVE && visibility = PUBLIC && listingStatus = LISTED}로
 * 수렴한다.
 *
 * <p>의미적으로 dev의 {@code DevRecipeQueryPredicates.publicListedActive}와 동일. dev는 추가로 viewableBy/ownerVisible까지
 * 가지지만 운영 코드는 단일 의미라 헬퍼 1개만 둔다.
 */
public final class RecipeQueryPredicates {

    private RecipeQueryPredicates() {}

    /** discovery — lifecycle=ACTIVE && visibility=PUBLIC && listing=LISTED */
    public static BooleanExpression discoverable(QRecipe recipe) {
        return recipe.lifecycleStatus.eq(RecipeLifecycleStatus.ACTIVE)
                .and(recipe.visibility.eq(RecipeVisibility.PUBLIC))
                .and(recipe.listingStatus.eq(RecipeListingStatus.LISTED));
    }
}
