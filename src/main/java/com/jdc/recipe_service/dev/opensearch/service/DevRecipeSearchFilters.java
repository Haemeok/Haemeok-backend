package com.jdc.recipe_service.dev.opensearch.service;

import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import org.opensearch.index.query.BoolQueryBuilder;
import org.opensearch.index.query.QueryBuilders;
import org.springframework.lang.Nullable;

/**
 * Dev V3 레시피 접근 가능 조건의 OpenSearch BoolQuery 표현.
 *
 * 의미 정의는 {@link com.jdc.recipe_service.dev.policy.recipe.DevRecipeAccessPolicy}에 있다.
 * 이 클래스는 그 의미를 OpenSearch keyword filter로 옮긴 어댑터다. 새 분기를 추가/수정할 때는 정책
 * 클래스를 먼저 바꾸고 같은 분기를 여기에 반영한다.
 *
 * 필터는 dev 인덱스 필드명({@code visibility}, {@code listingStatus}, {@code lifecycleStatus})을 가정하며,
 * 이 필드들은 {@link com.jdc.recipe_service.dev.opensearch.dto.DevRecipeDocument}에서 추가된다 (Batch 2).
 *
 * 사용 예 (A2에서):
 *   BoolQueryBuilder query = QueryBuilders.boolQuery()
 *           .must(textQuery)
 *           .filter(DevRecipeSearchFilters.publicListedActiveFilter());
 */
public final class DevRecipeSearchFilters {

    /** dev document field names — DevRecipeDocument와 일치 유지 */
    public static final String FIELD_LIFECYCLE_STATUS = "lifecycleStatus";
    public static final String FIELD_VISIBILITY = "visibility";
    public static final String FIELD_LISTING_STATUS = "listingStatus";
    public static final String FIELD_USER_ID = "userId";

    private DevRecipeSearchFilters() {}

    /** lifecycle=ACTIVE && visibility=PUBLIC && listing=LISTED — 검색/목록의 기본 가시성 필터 */
    public static BoolQueryBuilder publicListedActiveFilter() {
        return QueryBuilders.boolQuery()
                .filter(QueryBuilders.termQuery(FIELD_LIFECYCLE_STATUS, RecipeLifecycleStatus.ACTIVE.name()))
                .filter(QueryBuilders.termQuery(FIELD_VISIBILITY, RecipeVisibility.PUBLIC.name()))
                .filter(QueryBuilders.termQuery(FIELD_LISTING_STATUS, RecipeListingStatus.LISTED.name()));
    }

    /**
     * viewable — 단건 상세/저장/상호작용용. ACTIVE && (owner OR PUBLIC). listingStatus 무시 →
     * PUBLIC+UNLISTED(link-only)도 누구나 접근 가능.
     *
     * <ul>
     *   <li>viewerId == null → ACTIVE && PUBLIC (listingStatus 무관)</li>
     *   <li>viewerId != null → ACTIVE && (owner OR PUBLIC)</li>
     * </ul>
     */
    public static BoolQueryBuilder viewableByFilter(@Nullable Long viewerId) {
        BoolQueryBuilder activeOnly = QueryBuilders.boolQuery()
                .filter(QueryBuilders.termQuery(FIELD_LIFECYCLE_STATUS, RecipeLifecycleStatus.ACTIVE.name()));

        if (viewerId == null) {
            return activeOnly
                    .filter(QueryBuilders.termQuery(FIELD_VISIBILITY, RecipeVisibility.PUBLIC.name()));
        }

        BoolQueryBuilder isPublic = QueryBuilders.boolQuery()
                .filter(QueryBuilders.termQuery(FIELD_VISIBILITY, RecipeVisibility.PUBLIC.name()));
        BoolQueryBuilder isOwner = QueryBuilders.boolQuery()
                .filter(QueryBuilders.termQuery(FIELD_USER_ID, viewerId));

        return activeOnly.filter(QueryBuilders.boolQuery().should(isOwner).should(isPublic).minimumShouldMatch(1));
    }

    /**
     * owner-only — 내 레시피 목록 등. ACTIVE && owner.
     * viewerId == null이면 항상 0건이 되도록 매칭 불가능 필터를 반환 (anonymous 안전).
     */
    public static BoolQueryBuilder ownerVisibleFilter(@Nullable Long viewerId) {
        if (viewerId == null) {
            // 매칭 불가능 — 음수 id로 term query (id가 음수일 일 없음). 빈 결과 강제.
            return QueryBuilders.boolQuery()
                    .filter(QueryBuilders.termQuery(FIELD_USER_ID, -1L));
        }
        return QueryBuilders.boolQuery()
                .filter(QueryBuilders.termQuery(FIELD_LIFECYCLE_STATUS, RecipeLifecycleStatus.ACTIVE.name()))
                .filter(QueryBuilders.termQuery(FIELD_USER_ID, viewerId));
    }

    /**
     * @deprecated PUBLIC+UNLISTED를 link-only로 허용하지 않는 구 정책 (V1 호환용).
     * 의미별 필터({@link #publicListedActiveFilter}/{@link #viewableByFilter}/{@link #ownerVisibleFilter})로 점진 교체.
     *
     * <p>구 동작: ACTIVE && (owner OR (PUBLIC && LISTED)).
     */
    @Deprecated
    public static BoolQueryBuilder accessibleByFilter(@Nullable Long viewerId) {
        BoolQueryBuilder activeOnly = QueryBuilders.boolQuery()
                .filter(QueryBuilders.termQuery(FIELD_LIFECYCLE_STATUS, RecipeLifecycleStatus.ACTIVE.name()));

        if (viewerId == null) {
            return activeOnly
                    .filter(QueryBuilders.termQuery(FIELD_VISIBILITY, RecipeVisibility.PUBLIC.name()))
                    .filter(QueryBuilders.termQuery(FIELD_LISTING_STATUS, RecipeListingStatus.LISTED.name()));
        }

        BoolQueryBuilder publicListed = QueryBuilders.boolQuery()
                .filter(QueryBuilders.termQuery(FIELD_VISIBILITY, RecipeVisibility.PUBLIC.name()))
                .filter(QueryBuilders.termQuery(FIELD_LISTING_STATUS, RecipeListingStatus.LISTED.name()));
        BoolQueryBuilder isOwner = QueryBuilders.boolQuery()
                .filter(QueryBuilders.termQuery(FIELD_USER_ID, viewerId));

        return activeOnly.filter(QueryBuilders.boolQuery().should(isOwner).should(publicListed).minimumShouldMatch(1));
    }
}
