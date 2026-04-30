package com.jdc.recipe_service.dev.opensearch.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import org.hashids.Hashids;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.opensearch.index.query.BoolQueryBuilder;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * DevRecipeQueryBuilder의 BoolQuery 직렬화 invariant 검증.
 *
 * 핵심:
 *  1. 운영의 {@code isPrivate=false} term이 절대 안 들어감 (RESTRICTED 누수 차단의 회귀 게이트키퍼)
 *  2. anonymous: ACTIVE+PUBLIC+LISTED 필터만, userId term 없음
 *  3. authenticated: owner 분기 + minimum_should_match=1
 *  4. title 검색: keyword query (title.keyword/match/phrase/prefix/infix + youtubeChannelName) 추가
 *
 * 의미적 정확성은 통합 테스트(실제 OpenSearch index)에서 검증. 여기서는 toString() 직렬화로 분기/필드명을
 * 가볍게 잠근다 (DevRecipeSearchFiltersTest와 동일 패턴).
 */
class DevRecipeQueryBuilderTest {

    private DevRecipeQueryBuilder builder;

    @BeforeEach
    void setUp() {
        builder = new DevRecipeQueryBuilder(new Hashids("test-salt", 8));
    }

    @Test
    @DisplayName("isPrivate term은 어떤 호출에서든 절대 안 들어감 (운영 V2 isPrivate=false 대체 invariant)")
    void buildSearchQuery_neverIncludesIsPrivateTerm() {
        RecipeSearchCondition withTitle = new RecipeSearchCondition();
        withTitle.setTitle("김치찌개");

        BoolQueryBuilder q1 = builder.buildSearchQuery(withTitle, null);
        BoolQueryBuilder q2 = builder.buildSearchQuery(withTitle, 42L);
        BoolQueryBuilder q3 = builder.buildSearchQuery(new RecipeSearchCondition(), null);

        // 운영의 isPrivate=false term이 dev에서 사라지고 4-enum 정책으로 대체됨.
        // 이 invariant가 깨지면 RESTRICTED 누수 가능성 직결 → 회귀 시 즉시 잡힘.
        assertThat(q1.toString()).doesNotContain("\"isPrivate\"");
        assertThat(q2.toString()).doesNotContain("\"isPrivate\"");
        assertThat(q3.toString()).doesNotContain("\"isPrivate\"");
    }

    @Test
    @DisplayName("anonymous: lifecycleStatus=ACTIVE + visibility=PUBLIC + listingStatus=LISTED 모두 들어감, userId 없음")
    void buildSearchQuery_anonymous_appliesPublicListedActive_noUserIdTerm() {
        BoolQueryBuilder q = builder.buildSearchQuery(new RecipeSearchCondition(), null);
        String json = q.toString();

        assertThat(json).contains("lifecycleStatus");
        assertThat(json).contains("ACTIVE");
        assertThat(json).contains("visibility");
        assertThat(json).contains("PUBLIC");
        assertThat(json).contains("listingStatus");
        assertThat(json).contains("LISTED");
        assertThat(json).doesNotContain("userId");
    }

    @Test
    @DisplayName("authenticated viewer: owner 분기 (userId term + should + minimum_should_match=\"1\")")
    void buildSearchQuery_authenticated_includesOwnerBranch() {
        Long viewerId = 42L;
        BoolQueryBuilder q = builder.buildSearchQuery(new RecipeSearchCondition(), viewerId);
        String json = q.toString();

        assertThat(json).contains("userId");
        assertThat(json).contains("42");
        assertThat(json).containsIgnoringCase("should");
        // minimum_should_match=1 누락 시 should가 scoring-only로 빠져 RESTRICTED 누수 위험
        assertThat(json).contains("minimum_should_match");
        assertThat(json).contains("\"1\"");
        // owner 분기를 위해 PUBLIC + LISTED는 should 안에 살아있어야 함
        assertThat(json).contains("PUBLIC");
        assertThat(json).contains("LISTED");
    }

    @Test
    @DisplayName("title 검색: keyword query (title.keyword/prefix/infix + youtubeChannelName) 모두 must로 추가")
    void buildSearchQuery_title_includesKeywordBoolQuery() {
        RecipeSearchCondition cond = new RecipeSearchCondition();
        cond.setTitle("김치찌개");
        BoolQueryBuilder q = builder.buildSearchQuery(cond, null);
        String json = q.toString();

        assertThat(json).contains("title.keyword");
        assertThat(json).contains("title.prefix");
        assertThat(json).contains("title.infix");
        assertThat(json).contains("youtubeChannelName");
        assertThat(json).contains("김치찌개");
    }

    @Test
    @DisplayName("title 없음 + viewer 없음: accessibleByFilter만 포함 (title keyword query는 없음)")
    void buildSearchQuery_noTitle_onlyAccessFilter() {
        BoolQueryBuilder q = builder.buildSearchQuery(new RecipeSearchCondition(), null);
        String json = q.toString();

        // title keyword 분기는 없어야 함
        assertThat(json).doesNotContain("title.prefix");
        assertThat(json).doesNotContain("title.infix");
        // 접근 정책은 항상
        assertThat(json).contains("ACTIVE");
        assertThat(json).contains("PUBLIC");
        assertThat(json).contains("LISTED");
    }
}
