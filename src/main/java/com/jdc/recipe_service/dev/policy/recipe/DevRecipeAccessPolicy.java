package com.jdc.recipe_service.dev.policy.recipe;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import org.springframework.lang.Nullable;

/**
 * Dev V3 레시피 접근 가능 여부의 단일 의미 정의 (source of truth).
 *
 * 운영 V1/V2는 {@code isPrivate=false} 만으로 공개 여부를 판단하지만, dev V3는 4-enum 매트릭스를 사용한다:
 *   - lifecycleStatus: ACTIVE / HIDDEN / BANNED / DELETED
 *   - visibility:      PUBLIC / PRIVATE / RESTRICTED
 *   - listingStatus:   LISTED / UNLISTED
 *
 * 이 클래스는 in-memory 의미만 정의한다. 같은 의미의 SQL/OpenSearch 표현은 아래 두 어댑터로 분리된다:
 *   - {@code dev/repository/recipe/DevRecipeQueryPredicates} (QueryDSL)
 *   - {@code dev/opensearch/service/DevRecipeSearchFilters}  (OpenSearch BoolQuery)
 *
 * 세 어댑터는 항상 같은 의미를 표현해야 한다. 분기를 추가/수정할 때는 이 클래스의 정의를 먼저 바꾸고
 * 다른 두 곳에 동일 분기를 반영한다.
 */
public final class DevRecipeAccessPolicy {

    private DevRecipeAccessPolicy() {}

    // -----------------------------------------------------------------------
    // publicListedActive — 익명/타인이 검색·목록에서 볼 수 있는 조건
    // -----------------------------------------------------------------------

    /** lifecycle=ACTIVE && visibility=PUBLIC && listing=LISTED */
    public static boolean isPublicListedActive(Recipe recipe) {
        return isPublicListedActive(
                recipe.getLifecycleStatus(),
                recipe.getVisibility(),
                recipe.getListingStatus());
    }

    /** entity 없이 enum 조합만으로 의미 검증 (테스트/매트릭스 용). */
    public static boolean isPublicListedActive(RecipeLifecycleStatus lifecycle,
                                                RecipeVisibility visibility,
                                                RecipeListingStatus listing) {
        return lifecycle == RecipeLifecycleStatus.ACTIVE
                && visibility == RecipeVisibility.PUBLIC
                && listing == RecipeListingStatus.LISTED;
    }

    // -----------------------------------------------------------------------
    // accessibleBy — viewer 기준 단건 접근 가능 여부 (owner 분기 포함)
    // -----------------------------------------------------------------------

    /**
     * viewer가 이 레시피를 볼 수 있는가.
     *  - viewer가 owner이고 lifecycle=ACTIVE면 visibility/listing 무관하게 접근 (PRIVATE/RESTRICTED 모두 owner 자신은 봄)
     *  - 그 외에는 PUBLIC + LISTED + ACTIVE만 허용
     *  - lifecycle이 HIDDEN/BANNED/DELETED면 owner도 접근 거부 (admin 조치 우회 방지)
     *
     * @param viewerId 비로그인이면 null
     */
    public static boolean isAccessibleBy(Recipe recipe, @Nullable Long viewerId) {
        Long ownerId = (recipe.getUser() != null) ? recipe.getUser().getId() : null;
        return isAccessibleBy(
                recipe.getLifecycleStatus(),
                recipe.getVisibility(),
                recipe.getListingStatus(),
                viewerId,
                ownerId);
    }

    /** entity 없이 enum + id 조합만으로 의미 검증 (테스트/매트릭스 용). */
    public static boolean isAccessibleBy(RecipeLifecycleStatus lifecycle,
                                          RecipeVisibility visibility,
                                          RecipeListingStatus listing,
                                          @Nullable Long viewerId,
                                          @Nullable Long ownerId) {
        // non-ACTIVE는 owner도 접근 거부 — admin이 HIDDEN/BANNED 처리한 레시피를 owner가 우회로 보지 못하게
        if (lifecycle != RecipeLifecycleStatus.ACTIVE) return false;

        boolean isOwner = viewerId != null && ownerId != null && viewerId.equals(ownerId);
        if (isOwner) return true;

        return visibility == RecipeVisibility.PUBLIC && listing == RecipeListingStatus.LISTED;
    }
}
