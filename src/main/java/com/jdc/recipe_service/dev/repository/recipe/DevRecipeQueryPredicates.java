package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.domain.entity.QRecipe;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.querydsl.core.types.dsl.BooleanExpression;
import org.springframework.lang.Nullable;

/**
 * Dev V3 레시피 접근 가능 조건의 QueryDSL 표현.
 *
 * 의미 정의는 {@link com.jdc.recipe_service.dev.policy.recipe.DevRecipeAccessPolicy}에 있다.
 * 이 클래스는 그 의미를 SQL where절로 옮긴 어댑터일 뿐이고, 분기를 추가/수정할 때는 항상 정책 클래스를
 * 먼저 바꾸고 같은 분기를 여기에 반영한다.
 *
 * 사용 예 (A2/A3에서):
 *   .where(DevRecipeQueryPredicates.publicListedActive(QRecipe.recipe))
 *   .where(DevRecipeQueryPredicates.accessibleBy(QRecipe.recipe, viewerId))
 *
 * accessibleBy는 owner 분기를 위해 user join이 필요할 수 있다. 호출처가 join을 보장한다는 가정으로
 * QRecipe.user.id를 직접 참조한다.
 */
public final class DevRecipeQueryPredicates {

    private DevRecipeQueryPredicates() {}

    /** lifecycle=ACTIVE && visibility=PUBLIC && listing=LISTED */
    public static BooleanExpression publicListedActive(QRecipe recipe) {
        return recipe.lifecycleStatus.eq(RecipeLifecycleStatus.ACTIVE)
                .and(recipe.visibility.eq(RecipeVisibility.PUBLIC))
                .and(recipe.listingStatus.eq(RecipeListingStatus.LISTED));
    }

    /**
     * viewer 기준 접근 가능 조건.
     *  - viewerId == null → publicListedActive와 동일
     *  - viewerId != null → ACTIVE && (owner OR (PUBLIC && LISTED))
     *
     * non-ACTIVE는 owner도 접근 거부 (정책과 일치).
     */
    public static BooleanExpression accessibleBy(QRecipe recipe, @Nullable Long viewerId) {
        BooleanExpression activeOnly = recipe.lifecycleStatus.eq(RecipeLifecycleStatus.ACTIVE);
        BooleanExpression publicListed = recipe.visibility.eq(RecipeVisibility.PUBLIC)
                .and(recipe.listingStatus.eq(RecipeListingStatus.LISTED));

        if (viewerId == null) {
            return activeOnly.and(publicListed);
        }
        BooleanExpression isOwner = recipe.user.id.eq(viewerId);
        return activeOnly.and(isOwner.or(publicListed));
    }
}
