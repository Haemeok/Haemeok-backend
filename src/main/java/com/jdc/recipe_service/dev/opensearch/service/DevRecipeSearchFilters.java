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
     * viewer 기준 접근 가능 필터.
     *  - viewerId == null → publicListedActive와 동일
     *  - viewerId != null → ACTIVE && (owner OR (PUBLIC && LISTED))
     *
     * 호출처는 dev document에 {@code userId} 필드(owner id)가 있다고 가정한다 (DevRecipeDocument에서 추가).
     */
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
