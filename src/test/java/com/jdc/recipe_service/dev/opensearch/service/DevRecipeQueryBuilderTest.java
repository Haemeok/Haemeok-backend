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
 * <p>V1.x 정책: 검색은 discovery 단일 정책 — viewer와 무관하게 publicListedActiveFilter만 적용.
 * owner의 자기 PRIVATE/UNLISTED는 검색에 섞이지 않는다 (link-only는 직접 링크로만 접근).
 *
 * <p>핵심 invariant:
 * <ol>
 *   <li>운영의 {@code isPrivate=false} term이 절대 안 들어감 (RESTRICTED/UNLISTED 누수 차단의 회귀 게이트키퍼)</li>
 *   <li>anonymous/authenticated 모두 ACTIVE+PUBLIC+LISTED 필터만, userId term/owner 분기 없음</li>
 *   <li>title 검색: keyword query (title.keyword/match/phrase/prefix/infix + youtubeChannelName) 추가</li>
 * </ol>
 *
 * <p>의미적 정확성은 통합 테스트(실제 OpenSearch index)에서 검증. 여기서는 toString() 직렬화로 분기/필드명을
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
    @DisplayName("authenticated viewer: viewer 분기 없이 publicListedActive만 — userId term/owner 분기 없음 (link-only는 검색에서 빠짐)")
    void buildSearchQuery_authenticated_appliesPublicListedActive_noOwnerBranch() {
        Long viewerId = 42L;
        BoolQueryBuilder q = builder.buildSearchQuery(new RecipeSearchCondition(), viewerId);
        String json = q.toString();

        // V1.x 정책: 검색은 discovery 단일 정책 — viewer 무관하게 publicListedActive만.
        // owner branch가 들어가면 owner의 자기 PRIVATE/UNLISTED 글이 검색에 섞임 (link-only 정책 위반).
        // 핵심 게이트: userId term이 없고 viewerId 값이 직렬화에 등장하지 않으며, should branch가 활성화될 때만 붙는
        // minimum_should_match도 없어야 한다 (BoolQuery의 should[]는 항상 빈 array로 출력될 수 있어 should 자체는 검사 X).
        assertThat(json).doesNotContain("userId");
        assertThat(json).doesNotContain("\"42\"");
        assertThat(json).doesNotContain("minimum_should_match");
        // 접근 필터: ACTIVE + PUBLIC + LISTED (viewer 무관)
        assertThat(json).contains("ACTIVE");
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
