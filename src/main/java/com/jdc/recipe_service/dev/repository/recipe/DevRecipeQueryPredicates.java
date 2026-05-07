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
 * <p>의미 정의는 {@link com.jdc.recipe_service.dev.policy.recipe.DevRecipeAccessPolicy}에 있다.
 * 이 클래스는 그 의미를 SQL where절로 옮긴 어댑터일 뿐이고, 분기를 추가/수정할 때는 항상 정책 클래스를
 * 먼저 바꾸고 같은 분기를 여기에 반영한다.
 *
 * <p>의미별 predicate (V1.x):
 * <ul>
 *   <li>{@link #publicListedActive} — discovery (검색/추천/타인 프로필 등). ACTIVE + PUBLIC + LISTED.</li>
 *   <li>{@link #viewableBy} — 단건 상세/저장/상호작용. ACTIVE + (owner OR PUBLIC). listingStatus 무시.</li>
 *   <li>{@link #ownerVisible} — 내 레시피 목록 등. ACTIVE + owner.</li>
 * </ul>
 *
 * <p>owner 분기를 위해 user join이 필요할 수 있다. 호출처가 join을 보장한다는 가정으로
 * QRecipe.user.id를 직접 참조한다.
 */
public final class DevRecipeQueryPredicates {

    private DevRecipeQueryPredicates() {}

    /** discovery — lifecycle=ACTIVE && visibility=PUBLIC && listing=LISTED */
    public static BooleanExpression publicListedActive(QRecipe recipe) {
        return recipe.lifecycleStatus.eq(RecipeLifecycleStatus.ACTIVE)
                .and(recipe.visibility.eq(RecipeVisibility.PUBLIC))
                .and(recipe.listingStatus.eq(RecipeListingStatus.LISTED));
    }

    /**
     * viewable — 단건 조회/저장/상호작용용. ACTIVE + (owner OR PUBLIC). listingStatus 무시 →
     * PUBLIC+UNLISTED(link-only)도 누구나 접근 가능.
     *
     * <ul>
     *   <li>viewerId == null → ACTIVE + PUBLIC (listingStatus 무관)</li>
     *   <li>viewerId != null → ACTIVE + (owner OR PUBLIC)</li>
     * </ul>
     */
    public static BooleanExpression viewableBy(QRecipe recipe, @Nullable Long viewerId) {
        BooleanExpression activeOnly = recipe.lifecycleStatus.eq(RecipeLifecycleStatus.ACTIVE);
        BooleanExpression isPublic = recipe.visibility.eq(RecipeVisibility.PUBLIC);

        if (viewerId == null) {
            return activeOnly.and(isPublic);
        }
        BooleanExpression isOwner = recipe.user.id.eq(viewerId);
        return activeOnly.and(isOwner.or(isPublic));
    }

    /**
     * owner-only — 내 레시피 목록 등. ACTIVE + owner.
     * viewerId == null이면 항상 false인 표현 ({@code 1=0})을 반환해 호출처가 분기 없이 사용 가능.
     */
    public static BooleanExpression ownerVisible(QRecipe recipe, @Nullable Long viewerId) {
        if (viewerId == null) {
            // 1=0 — 빈 결과 강제. 호출처가 anonymous에서도 안전하게 사용.
            return recipe.id.isNull().and(recipe.id.isNotNull());
        }
        return recipe.lifecycleStatus.eq(RecipeLifecycleStatus.ACTIVE)
                .and(recipe.user.id.eq(viewerId));
    }

    /**
     * @deprecated PUBLIC+UNLISTED를 link-only로 허용하지 않는 구 정책 (V1 호환용).
     * 의미별 predicate({@link #publicListedActive}/{@link #viewableBy}/{@link #ownerVisible})로 점진 교체.
     */
    @Deprecated
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
