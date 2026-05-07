package com.jdc.recipe_service.dev.opensearch.service;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.opensearch.index.query.BoolQueryBuilder;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevRecipeSearchFilters 구조 검증.
 *
 * BoolQuery 의 의미적 정확성은 통합 테스트(A2 검색 API + 실제 OpenSearch index)에서 보장된다.
 * 여기서는 toString() 직렬화에 invariant 키워드가 포함되는지로 분기/필드명/enum 값이 빠지지 않았는지만 가볍게 잡는다.
 */
class DevRecipeSearchFiltersTest {

    @Test
    @DisplayName("publicListedActiveFilter: ACTIVE + PUBLIC + LISTED 세 term filter가 모두 들어 있다")
    void publicListedActive_containsAllThreeTerms() {
        BoolQueryBuilder filter = DevRecipeSearchFilters.publicListedActiveFilter();
        String json = filter.toString();

        assertThat(json).contains(DevRecipeSearchFilters.FIELD_LIFECYCLE_STATUS);
        assertThat(json).contains(DevRecipeSearchFilters.FIELD_VISIBILITY);
        assertThat(json).contains(DevRecipeSearchFilters.FIELD_LISTING_STATUS);
        assertThat(json).contains("ACTIVE");
        assertThat(json).contains("PUBLIC");
        assertThat(json).contains("LISTED");
        // userId 필드는 publicListed에는 들어가면 안 됨 (owner 분기 없음)
        assertThat(json).doesNotContain(DevRecipeSearchFilters.FIELD_USER_ID);
    }

    @Test
    @DisplayName("accessibleByFilter(null): publicListedActive와 동일 시맨틱 (ACTIVE + PUBLIC + LISTED, owner 분기 없음)")
    void accessibleBy_anonymous_matchesPublicListedActive() {
        BoolQueryBuilder filter = DevRecipeSearchFilters.accessibleByFilter(null);
        String json = filter.toString();

        assertThat(json).contains("ACTIVE");
        assertThat(json).contains("PUBLIC");
        assertThat(json).contains("LISTED");
        assertThat(json).doesNotContain(DevRecipeSearchFilters.FIELD_USER_ID);
    }

    @Test
    @DisplayName("accessibleByFilter(viewerId): owner 분기 포함 (userId term + should bool + minimum_should_match=1)")
    void accessibleBy_authenticated_includesOwnerBranch() {
        Long viewerId = 42L;
        BoolQueryBuilder filter = DevRecipeSearchFilters.accessibleByFilter(viewerId);
        String json = filter.toString();

        // 필수: ACTIVE만 무조건 filter
        assertThat(json).contains(DevRecipeSearchFilters.FIELD_LIFECYCLE_STATUS);
        assertThat(json).contains("ACTIVE");
        // owner 분기: userId 필드 + viewerId 값
        assertThat(json).contains(DevRecipeSearchFilters.FIELD_USER_ID);
        assertThat(json).contains("42");
        // PUBLIC + LISTED는 should branch에 살아있어야 (non-owner 케이스)
        assertThat(json).contains("PUBLIC");
        assertThat(json).contains("LISTED");
        // owner OR public-listed 분기를 위해 should + minimum_should_match=1 둘 다 필요.
        // (둘 중 하나만 있으면 OpenSearch에서 should는 점수만 영향 주고 필터로 동작 안 함 → RESTRICTED 누수 위험)
        assertThat(json).containsIgnoringCase("should");
        assertThat(json).contains("minimum_should_match");
        assertThat(json).contains("\"1\"");
    }

    // =========================================================================
    // V1.x — 의미별 필터 (link-only 정책)
    // =========================================================================

    @Test
    @DisplayName("viewableByFilter(null): ACTIVE + PUBLIC만 — listingStatus는 의도적으로 빠짐 (PUBLIC+UNLISTED도 link로 접근)")
    void viewableBy_anonymous_omitsListingStatus() {
        BoolQueryBuilder filter = DevRecipeSearchFilters.viewableByFilter(null);
        String json = filter.toString();

        assertThat(json).contains(DevRecipeSearchFilters.FIELD_LIFECYCLE_STATUS);
        assertThat(json).contains(DevRecipeSearchFilters.FIELD_VISIBILITY);
        assertThat(json).contains("ACTIVE");
        assertThat(json).contains("PUBLIC");
        // 핵심 invariant: listingStatus 필드는 viewable에서 절대 들어가면 안 된다 (link-only 허용 위해 의도적 누락)
        assertThat(json).doesNotContain(DevRecipeSearchFilters.FIELD_LISTING_STATUS);
        assertThat(json).doesNotContain("LISTED");
        assertThat(json).doesNotContain(DevRecipeSearchFilters.FIELD_USER_ID);
    }

    @Test
    @DisplayName("viewableByFilter(viewerId): owner 분기 + listingStatus 빠짐 (link-only 핵심)")
    void viewableBy_authenticated_includesOwnerBranchWithoutListingStatus() {
        Long viewerId = 42L;
        BoolQueryBuilder filter = DevRecipeSearchFilters.viewableByFilter(viewerId);
        String json = filter.toString();

        assertThat(json).contains("ACTIVE");
        assertThat(json).contains(DevRecipeSearchFilters.FIELD_USER_ID);
        assertThat(json).contains("42");
        assertThat(json).contains("PUBLIC");
        // viewable의 핵심 invariant: listingStatus는 owner 분기에도 non-owner 분기에도 절대 안 들어간다.
        // (들어가면 PUBLIC+UNLISTED가 OpenSearch 검색에서 빠져서 link-only 정책 위반)
        assertThat(json).doesNotContain(DevRecipeSearchFilters.FIELD_LISTING_STATUS);
        assertThat(json).doesNotContain("LISTED");
        // owner OR public 분기를 위해 should + minimum_should_match=1 필요
        assertThat(json).containsIgnoringCase("should");
        assertThat(json).contains("minimum_should_match");
        assertThat(json).contains("\"1\"");
    }

    @Test
    @DisplayName("ownerVisibleFilter(viewerId): ACTIVE + userId만 — visibility/listing 차원 빠짐 (owner-only)")
    void ownerVisible_authenticated_filtersByOwnerActiveOnly() {
        Long viewerId = 42L;
        BoolQueryBuilder filter = DevRecipeSearchFilters.ownerVisibleFilter(viewerId);
        String json = filter.toString();

        assertThat(json).contains(DevRecipeSearchFilters.FIELD_LIFECYCLE_STATUS);
        assertThat(json).contains("ACTIVE");
        assertThat(json).contains(DevRecipeSearchFilters.FIELD_USER_ID);
        assertThat(json).contains("42");
        // ownerVisible은 owner 자신의 모든 ACTIVE 글을 보여주므로 visibility/listing 차원에 의존하면 안 된다.
        assertThat(json).doesNotContain(DevRecipeSearchFilters.FIELD_VISIBILITY);
        assertThat(json).doesNotContain("PUBLIC");
        assertThat(json).doesNotContain(DevRecipeSearchFilters.FIELD_LISTING_STATUS);
        assertThat(json).doesNotContain("LISTED");
    }

    @Test
    @DisplayName("ownerVisibleFilter(null): anonymous는 매칭 불가능한 음수 id로 0건 강제 (안전 가드)")
    void ownerVisible_anonymous_matchesNothing() {
        BoolQueryBuilder filter = DevRecipeSearchFilters.ownerVisibleFilter(null);
        String json = filter.toString();

        // anonymous에 ownerVisible은 의미상 0건 — 음수 id term으로 어떤 row와도 매칭 안 됨.
        assertThat(json).contains(DevRecipeSearchFilters.FIELD_USER_ID);
        assertThat(json).contains("-1");
    }
}
