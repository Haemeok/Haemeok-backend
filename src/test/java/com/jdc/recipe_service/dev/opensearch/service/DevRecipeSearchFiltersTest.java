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
}
