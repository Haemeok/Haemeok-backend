package com.jdc.recipe_service.dev.policy.recipe;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import org.springframework.lang.Nullable;

/**
 * Dev V3 레시피 접근 가능 여부의 단일 의미 정의 (source of truth).
 *
 * <p>운영 V1/V2는 {@code isPrivate=false} 만으로 공개 여부를 판단하지만, dev V3는 4-enum 매트릭스를 사용한다:
 * <ul>
 *   <li>lifecycleStatus: ACTIVE / HIDDEN / BANNED / DELETED</li>
 *   <li>visibility:      PUBLIC / PRIVATE / RESTRICTED (deprecated)</li>
 *   <li>listingStatus:   LISTED / UNLISTED</li>
 * </ul>
 *
 * <p><b>의미별 메서드 (V1.x — link-only 정책 도입)</b>:
 * <ul>
 *   <li>{@link #isPublicListedActive} (= isDiscoverable) — 검색/추천/타인 프로필 등 discovery 노출 조건.
 *       ACTIVE + PUBLIC + LISTED. PUBLIC+UNLISTED는 검색에서 빠짐.</li>
 *   <li>{@link #isViewableBy} — 단건 상세/저장/상호작용. ACTIVE + (owner OR PUBLIC). listingStatus 무시 →
 *       PUBLIC+UNLISTED(link-only) 글도 누구나 볼 수 있음.</li>
 *   <li>{@link #isOwnerVisibleBy} — 내 레시피 목록 등 owner-only 화면. ACTIVE + owner.</li>
 * </ul>
 *
 * <p>이 클래스는 in-memory 의미만 정의한다. 같은 의미의 SQL/OpenSearch 표현은 아래 두 어댑터로 분리된다 — 셋 다 동일 정책을 표현:
 * <ul>
 *   <li>{@code dev/repository/recipe/DevRecipeQueryPredicates} (QueryDSL) — viewableBy/ownerVisible/publicListedActive</li>
 *   <li>{@code dev/opensearch/service/DevRecipeSearchFilters}  (OpenSearch BoolQuery) — viewableByFilter/ownerVisibleFilter/publicListedActiveFilter</li>
 * </ul>
 *
 * <p>구 {@code accessibleBy} / {@code accessibleByFilter}는 V1 호환 위해 deprecated 채로 보존되며, 신규 호출은 의미별 메서드를 사용한다.
 * 분기를 추가/수정할 때는 이 클래스의 정의를 먼저 바꾸고 다른 두 곳에 동일 분기를 반영한다.
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
    // viewableBy — 상세/저장/상호작용 접근 가능 여부 (owner 분기 + PUBLIC link-only 허용)
    // -----------------------------------------------------------------------

    /**
     * viewer가 이 레시피를 단건 조회/저장/상호작용할 수 있는가.
     * <ul>
     *   <li>viewer가 owner이고 lifecycle=ACTIVE면 visibility 무관하게 접근 (PRIVATE도 owner 자신은 봄)</li>
     *   <li>visibility=PUBLIC이고 lifecycle=ACTIVE면 누구나 접근 — listingStatus 무시
     *       (PUBLIC+UNLISTED도 link로 접근/저장/댓글/즐겨찾기 가능)</li>
     *   <li>lifecycle이 HIDDEN/BANNED/DELETED면 owner도 접근 거부 (admin 조치 우회 방지)</li>
     *   <li>visibility=RESTRICTED는 owner만 — 신규 row는 차단되지만 legacy row 호환을 위해 owner 접근은 허용</li>
     * </ul>
     *
     * @param viewerId 비로그인이면 null
     */
    public static boolean isViewableBy(Recipe recipe, @Nullable Long viewerId) {
        Long ownerId = (recipe.getUser() != null) ? recipe.getUser().getId() : null;
        return isViewableBy(
                recipe.getLifecycleStatus(),
                recipe.getVisibility(),
                viewerId,
                ownerId);
    }

    /** entity 없이 enum + id 조합만으로 의미 검증 (테스트/매트릭스 용). listingStatus는 의도적으로 받지 않는다. */
    public static boolean isViewableBy(RecipeLifecycleStatus lifecycle,
                                        RecipeVisibility visibility,
                                        @Nullable Long viewerId,
                                        @Nullable Long ownerId) {
        // non-ACTIVE는 owner도 접근 거부 — admin이 HIDDEN/BANNED 처리한 레시피를 owner가 우회로 보지 못하게
        if (lifecycle != RecipeLifecycleStatus.ACTIVE) return false;

        boolean isOwner = viewerId != null && ownerId != null && viewerId.equals(ownerId);
        if (isOwner) return true;

        // PUBLIC이면 listingStatus와 무관하게 접근 허용 (link-only/UNLISTED도 viewable).
        return visibility == RecipeVisibility.PUBLIC;
    }

    // -----------------------------------------------------------------------
    // ownerVisibleBy — 내 레시피 목록 등 owner-only 화면
    // -----------------------------------------------------------------------

    /**
     * viewer가 owner이고 lifecycle=ACTIVE인 레시피만 노출하는 화면용 (예: 내 레시피 목록).
     * 비로그인이거나 owner가 아니면 false. non-ACTIVE도 false.
     */
    public static boolean isOwnerVisibleBy(Recipe recipe, @Nullable Long viewerId) {
        Long ownerId = (recipe.getUser() != null) ? recipe.getUser().getId() : null;
        return isOwnerVisibleBy(recipe.getLifecycleStatus(), viewerId, ownerId);
    }

    /** entity 없이 enum + id 조합만으로 의미 검증 (테스트/매트릭스 용). */
    public static boolean isOwnerVisibleBy(RecipeLifecycleStatus lifecycle,
                                            @Nullable Long viewerId,
                                            @Nullable Long ownerId) {
        if (lifecycle != RecipeLifecycleStatus.ACTIVE) return false;
        return viewerId != null && ownerId != null && viewerId.equals(ownerId);
    }

    // -----------------------------------------------------------------------
    // accessibleBy (deprecated) — 구 정책: ACTIVE + (owner OR (PUBLIC && LISTED))
    // -----------------------------------------------------------------------

    /**
     * @deprecated PUBLIC+UNLISTED를 link-only로 허용하지 않는 구 정책. 의미별 메서드로 점진 교체.
     * <ul>
     *   <li>discovery(검색/추천/타인 프로필 등)는 {@link #isPublicListedActive}</li>
     *   <li>단건 상세/저장/상호작용은 {@link #isViewableBy} (PUBLIC+UNLISTED도 허용)</li>
     *   <li>owner-only 화면(내 레시피 목록 등)은 {@link #isOwnerVisibleBy}</li>
     * </ul>
     * 동작은 보존: ACTIVE + (owner OR (PUBLIC && LISTED)). 마이그레이션 완료 후 제거 예정.
     */
    @Deprecated
    public static boolean isAccessibleBy(Recipe recipe, @Nullable Long viewerId) {
        Long ownerId = (recipe.getUser() != null) ? recipe.getUser().getId() : null;
        return isAccessibleBy(
                recipe.getLifecycleStatus(),
                recipe.getVisibility(),
                recipe.getListingStatus(),
                viewerId,
                ownerId);
    }

    /** @deprecated {@link #isAccessibleBy(Recipe, Long)} 참고. */
    @Deprecated
    public static boolean isAccessibleBy(RecipeLifecycleStatus lifecycle,
                                          RecipeVisibility visibility,
                                          RecipeListingStatus listing,
                                          @Nullable Long viewerId,
                                          @Nullable Long ownerId) {
        if (lifecycle != RecipeLifecycleStatus.ACTIVE) return false;
        boolean isOwner = viewerId != null && ownerId != null && viewerId.equals(ownerId);
        if (isOwner) return true;
        return visibility == RecipeVisibility.PUBLIC && listing == RecipeListingStatus.LISTED;
    }
}
