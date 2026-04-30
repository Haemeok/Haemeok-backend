package com.jdc.recipe_service.dev.service.search;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDto;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDtoV2;
import com.jdc.recipe_service.dev.opensearch.service.DevRecipeQueryBuilder;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipePopularBudgetRepository;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeQueryRepositoryV2;
import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.util.SearchProperties;
import org.apache.lucene.search.TotalHits;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.opensearch.action.search.SearchRequest;
import org.opensearch.action.search.SearchResponse;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.index.query.BoolQueryBuilder;
import org.opensearch.index.query.QueryBuilders;
import org.opensearch.search.SearchHit;
import org.opensearch.search.SearchHits;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeSearchService engine 분기 + stale total 보정 검증.
 *
 * 분기 매트릭스:
 *  1. devIndex.enabled=false → 항상 QueryDSL (engine 무관) — dev alias 비어 사고 방지
 *  2. engine=querydsl → QueryDSL
 *  3. engine=opensearch + enabled → OpenSearch
 *  4. engine=auto + enabled + healthy + title 있음 → OpenSearch
 *  5. engine=auto + enabled + title 없음 → QueryDSL
 *  6. OpenSearch 호출 throw → QueryDSL fallback
 *
 * 추가 invariant:
 *  7. stale hit total 보정 — OS hits=3, DB returns 2 → content 2, total = OS total - 1, WARN log 1회
 *
 * 의미적으로 OpenSearch SearchResponse 체인이 깊어 mock chain 사용. 실제 OpenSearch 동작은 통합 영역.
 *
 * Strict mode 보존: 클래스 전체 LENIENT 대신 BeforeEach의 공통 queryBuilder stub만 lenient()로 좁혔다.
 * 이러면 향후 실수로 추가된 unused stub은 strict 모드가 즉시 잡아준다.
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeSearchServiceTest {

    @Mock RestHighLevelClient client;
    @Mock DevRecipeQueryBuilder queryBuilder;
    @Mock DevRecipeQueryRepositoryV2 devRecipeQueryRepository;
    @Mock DevRecipePopularBudgetRepository devRecipePopularBudgetRepository;

    private SearchProperties searchProperties;
    private DevRecipeSearchService service;

    private static final Long VIEWER_ID = 7L;
    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @BeforeEach
    void setUp() {
        searchProperties = new SearchProperties();
        searchProperties.setEngine("auto");
        searchProperties.getDevIndex().setEnabled(true);
        searchProperties.getDevIndex().setAlias("recipes_v3_dev");

        service = new DevRecipeSearchService(
                client, queryBuilder, devRecipeQueryRepository, devRecipePopularBudgetRepository, searchProperties);
        ReflectionTestUtils.setField(service, "bucketName", "test-bucket");
        ReflectionTestUtils.setField(service, "region", "ap-northeast-2");

        // queryBuilder는 모든 호출에서 빈 BoolQuery 반환 (실제 query 내용은 DevRecipeQueryBuilderTest에서 검증).
        // QueryDSL 분기 테스트는 이 stub을 안 쓰므로 lenient() — strict mode를 클래스 전체로 풀지 않고 좁힘.
        lenient().when(queryBuilder.buildSearchQuery(any(), any())).thenReturn(QueryBuilders.boolQuery());
    }

    private RecipeSearchCondition cond(String title) {
        RecipeSearchCondition c = new RecipeSearchCondition();
        if (title != null) c.setTitle(title);
        return c;
    }

    private void healthy(boolean state) {
        ReflectionTestUtils.setField(service, "isOpenSearchHealthy", state);
    }

    // ---------- Engine 분기 매트릭스 ----------

    @Test
    @DisplayName("devIndex.enabled=false: engine 무관 항상 QueryDSL (dev alias 비어 사고 방지)")
    void devIndexDisabled_alwaysQueryDsl() throws Exception {
        searchProperties.getDevIndex().setEnabled(false);
        searchProperties.setEngine("opensearch"); // 명시적으로 opensearch라도
        healthy(true);
        given(devRecipeQueryRepository.searchStatic(any(), any(), any())).willReturn(Page.empty(PAGE_10));

        service.searchRecipes(cond("김치"), PAGE_10, VIEWER_ID);

        verify(devRecipeQueryRepository).searchStatic(any(), eq(PAGE_10), eq(VIEWER_ID));
        verifyNoInteractions(client);
    }

    @Test
    @DisplayName("engine=querydsl: 항상 QueryDSL (OpenSearch healthy 무관)")
    void engineQueryDsl_alwaysQueryDsl() throws Exception {
        searchProperties.setEngine("querydsl");
        healthy(true);
        given(devRecipeQueryRepository.searchStatic(any(), any(), any())).willReturn(Page.empty(PAGE_10));

        service.searchRecipes(cond("김치"), PAGE_10, VIEWER_ID);

        verify(devRecipeQueryRepository).searchStatic(any(), eq(PAGE_10), eq(VIEWER_ID));
        verifyNoInteractions(client);
    }

    @Test
    @DisplayName("engine=opensearch + enabled: title 없어도 OpenSearch 호출")
    void engineOpenSearch_alwaysOpenSearch() throws Exception {
        searchProperties.setEngine("opensearch");
        healthy(true);
        stubEmptyOpenSearchResponse();

        service.searchRecipes(cond(null), PAGE_10, VIEWER_ID);

        verify(client).search(any(SearchRequest.class), eq(RequestOptions.DEFAULT));
        verify(devRecipeQueryRepository, never()).searchStatic(any(), any(), any());
    }

    @Test
    @DisplayName("engine=auto + healthy + title 있음: OpenSearch 호출 (운영 V2 동일 정책)")
    void engineAuto_healthyWithTitle_useOpenSearch() throws Exception {
        searchProperties.setEngine("auto");
        healthy(true);
        stubEmptyOpenSearchResponse();

        service.searchRecipes(cond("김치"), PAGE_10, VIEWER_ID);

        verify(client).search(any(SearchRequest.class), eq(RequestOptions.DEFAULT));
        verify(devRecipeQueryRepository, never()).searchStatic(any(), any(), any());
    }

    @Test
    @DisplayName("engine=auto + title 없음: QueryDSL (OpenSearch 호출 안 함)")
    void engineAuto_noTitle_useQueryDsl() throws Exception {
        searchProperties.setEngine("auto");
        healthy(true);
        given(devRecipeQueryRepository.searchStatic(any(), any(), any())).willReturn(Page.empty(PAGE_10));

        service.searchRecipes(cond(null), PAGE_10, VIEWER_ID);

        verify(devRecipeQueryRepository).searchStatic(any(), any(), any());
        verifyNoInteractions(client);
    }

    @Test
    @DisplayName("engine=auto + unhealthy: title 있어도 QueryDSL")
    void engineAuto_unhealthy_useQueryDsl() throws Exception {
        searchProperties.setEngine("auto");
        healthy(false);
        given(devRecipeQueryRepository.searchStatic(any(), any(), any())).willReturn(Page.empty(PAGE_10));

        service.searchRecipes(cond("김치"), PAGE_10, VIEWER_ID);

        verify(devRecipeQueryRepository).searchStatic(any(), any(), any());
        verifyNoInteractions(client);
    }

    @Test
    @DisplayName("OpenSearch 호출 throw: QueryDSL fallback (운영 V2 동일)")
    void openSearchThrow_fallbackToQueryDsl() throws Exception {
        searchProperties.setEngine("opensearch");
        healthy(true);
        given(client.search(any(SearchRequest.class), eq(RequestOptions.DEFAULT)))
                .willThrow(new IOException("opensearch dev cluster down"));
        given(devRecipeQueryRepository.searchStatic(any(), any(), any())).willReturn(Page.empty(PAGE_10));

        service.searchRecipes(cond("김치"), PAGE_10, VIEWER_ID);

        verify(client).search(any(SearchRequest.class), eq(RequestOptions.DEFAULT));
        verify(devRecipeQueryRepository).searchStatic(any(), any(), any());
    }

    // ---------- stale hit total 보정 ----------

    @Test
    @DisplayName("stale hit 보정: OS hits=3, DB returns 2 → content 2건, total = OS total(3) - dropped(1) = 2 + viewerId가 query builder까지 흐름")
    void staleHitTotalCorrection_dropsCountedFromTotal_andViewerIdFlowsToBuilder() throws Exception {
        searchProperties.setEngine("opensearch");
        healthy(true);

        // OpenSearch가 3개 hit 반환, total=3
        SearchHit hit1 = mockSearchHitWithId("101");
        SearchHit hit2 = mockSearchHitWithId("102");
        SearchHit hit3 = mockSearchHitWithId("103");
        stubOpenSearchResponse(new SearchHit[]{hit1, hit2, hit3}, 3L);

        // DB는 정책 검증 후 2개만 반환 (id=102가 stale RESTRICTED라 차단됐다고 가정)
        DevRecipeSimpleStaticDto dto1 = dtoWithId(101L, "pub1");
        DevRecipeSimpleStaticDto dto3 = dtoWithId(103L, "pub3");
        given(devRecipeQueryRepository.findAllByIds(eq(List.of(101L, 102L, 103L)), eq(VIEWER_ID)))
                .willReturn(List.of(dto1, dto3));

        Page<DevRecipeSimpleStaticDto> result = service.searchRecipes(cond("김치"), PAGE_10, VIEWER_ID);

        // content = 2 (stale 제거)
        assertThat(result.getContent()).hasSize(2);
        assertThat(result.getContent()).extracting(DevRecipeSimpleStaticDto::getId).containsExactly(101L, 103L);
        // total 보정: OS total(3) - dropped(1) = 2
        assertThat(result.getTotalElements())
                .as("같은 페이지의 stale hit은 OS total에서 차감되어야 함")
                .isEqualTo(2L);

        // 회귀 게이트: viewerId가 queryBuilder까지 정확히 흘러야 owner 분기가 BoolQuery에 반영됨.
        // null로 회귀하면 owner의 PRIVATE/RESTRICTED 검색이 깨져도 위 assertion만으로는 못 잡음.
        verify(queryBuilder).buildSearchQuery(any(RecipeSearchCondition.class), eq(VIEWER_ID));
    }

    @Test
    @DisplayName("정상: OS hits=2, DB returns 2 → content 2건, total = OS total(2) (보정 없음)")
    void noStaleHits_totalUnchanged() throws Exception {
        searchProperties.setEngine("opensearch");
        healthy(true);

        SearchHit hit1 = mockSearchHitWithId("201");
        SearchHit hit2 = mockSearchHitWithId("202");
        stubOpenSearchResponse(new SearchHit[]{hit1, hit2}, 2L);

        DevRecipeSimpleStaticDto dto1 = dtoWithId(201L, "a");
        DevRecipeSimpleStaticDto dto2 = dtoWithId(202L, "b");
        given(devRecipeQueryRepository.findAllByIds(eq(List.of(201L, 202L)), eq(VIEWER_ID)))
                .willReturn(List.of(dto1, dto2));

        Page<DevRecipeSimpleStaticDto> result = service.searchRecipes(cond("김치"), PAGE_10, VIEWER_ID);

        assertThat(result.getContent()).hasSize(2);
        assertThat(result.getTotalElements()).isEqualTo(2L);
    }

    @Test
    @DisplayName("OS hits=0: 빈 page 즉시 반환 (DB 호출 없음)")
    void openSearchEmptyHits_returnsEmptyPage_noDbCall() throws Exception {
        searchProperties.setEngine("opensearch");
        healthy(true);
        stubEmptyOpenSearchResponse();

        Page<DevRecipeSimpleStaticDto> result = service.searchRecipes(cond("김치"), PAGE_10, VIEWER_ID);

        assertThat(result.getContent()).isEmpty();
        verify(devRecipeQueryRepository, never()).findAllByIds(any(), any());
    }

    // ---------- helpers ----------

    private void stubEmptyOpenSearchResponse() throws IOException {
        stubOpenSearchResponse(new SearchHit[0], 0L);
    }

    private void stubOpenSearchResponse(SearchHit[] hits, long total) throws IOException {
        SearchHits searchHits = mock(SearchHits.class);
        given(searchHits.getHits()).willReturn(hits);
        // 빈 hits 케이스에서 service가 early return하면 getTotalHits()는 호출되지 않음 → 좁은 lenient.
        lenient().when(searchHits.getTotalHits()).thenReturn(new TotalHits(total, TotalHits.Relation.EQUAL_TO));

        SearchResponse response = mock(SearchResponse.class);
        given(response.getHits()).willReturn(searchHits);

        given(client.search(any(SearchRequest.class), eq(RequestOptions.DEFAULT))).willReturn(response);
    }

    private SearchHit mockSearchHitWithId(String id) {
        SearchHit hit = mock(SearchHit.class);
        given(hit.getId()).willReturn(id);
        return hit;
    }

    private DevRecipeSimpleStaticDto dtoWithId(Long id, String title) {
        DevRecipeSimpleStaticDto dto = DevRecipeSimpleStaticDto.builder()
                .id(id)
                .title(title)
                .build();
        return dto;
    }

    // =========================================================================
    // popular / budget — 운영 V2 미러 분기 검증
    // =========================================================================

    @Test
    @DisplayName("getPopularRecipes(weekly): weekly column path 호출, realtime path 호출 안 함")
    void getPopularRecipes_weekly_callsWeeklyColumnPath() {
        given(devRecipePopularBudgetRepository.findPopularDevWeekly(any())).willReturn(Page.empty(PAGE_10));

        service.getPopularRecipes("weekly", PAGE_10);

        verify(devRecipePopularBudgetRepository).findPopularDevWeekly(any());
        verify(devRecipePopularBudgetRepository, never()).findPopularDevByRealtimeCount(any(), any());
    }

    @Test
    @DisplayName("getPopularRecipes(period=null/blank): weekly path로 떨어짐 (default)")
    void getPopularRecipes_nullPeriod_defaultsToWeekly() {
        given(devRecipePopularBudgetRepository.findPopularDevWeekly(any())).willReturn(Page.empty(PAGE_10));

        service.getPopularRecipes(null, PAGE_10);
        service.getPopularRecipes("", PAGE_10);

        verify(devRecipePopularBudgetRepository, org.mockito.Mockito.times(2)).findPopularDevWeekly(any());
        verify(devRecipePopularBudgetRepository, never()).findPopularDevByRealtimeCount(any(), any());
    }

    @Test
    @DisplayName("getPopularRecipes(monthly): realtime path + startDate=now-30d 캡처")
    void getPopularRecipes_monthly_callsRealtimeWith30DaysAgo() {
        given(devRecipePopularBudgetRepository.findPopularDevByRealtimeCount(any(), any())).willReturn(Page.empty(PAGE_10));

        LocalDateTime before = LocalDateTime.now().minusDays(30).minusSeconds(5);
        service.getPopularRecipes("monthly", PAGE_10);
        LocalDateTime after = LocalDateTime.now().minusDays(30).plusSeconds(5);

        ArgumentCaptor<LocalDateTime> dateCaptor = ArgumentCaptor.forClass(LocalDateTime.class);
        verify(devRecipePopularBudgetRepository).findPopularDevByRealtimeCount(dateCaptor.capture(), any());
        verify(devRecipePopularBudgetRepository, never()).findPopularDevWeekly(any());
        // monthly = now - 30 days, 호출 시점 윈도우 안에 있어야 (race-free)
        assertThat(dateCaptor.getValue()).isBetween(before, after);
    }

    @Test
    @DisplayName("getPopularRecipes(all/unknown): realtime path + startDate=2000-01-01 (운영 V2 fallback semantics)")
    void getPopularRecipes_unknownPeriod_callsRealtimeWith2000Start() {
        given(devRecipePopularBudgetRepository.findPopularDevByRealtimeCount(any(), any())).willReturn(Page.empty(PAGE_10));

        service.getPopularRecipes("all", PAGE_10);
        service.getPopularRecipes("quarterly", PAGE_10); // 운영 V2 parity — unknown은 all 시맨틱

        ArgumentCaptor<LocalDateTime> dateCaptor = ArgumentCaptor.forClass(LocalDateTime.class);
        verify(devRecipePopularBudgetRepository, org.mockito.Mockito.times(2))
                .findPopularDevByRealtimeCount(dateCaptor.capture(), any());

        LocalDateTime expected = LocalDateTime.of(2000, 1, 1, 0, 0);
        assertThat(dateCaptor.getAllValues()).containsExactly(expected, expected);
    }

    @Test
    @DisplayName("getBudgetRecipes(maxCost=10000): top10 제외 list 그대로 전달, maxCost 그대로")
    void getBudgetRecipes_normalCase_passesArgsToBudgetRepo() {
        given(devRecipePopularBudgetRepository.findTop10PopularDevIds(any())).willReturn(List.of(1L, 2L, 3L));
        given(devRecipePopularBudgetRepository.findBudgetDev(eq(10000), eq(List.of(1L, 2L, 3L)), any()))
                .willReturn(Page.empty(PAGE_10));

        service.getBudgetRecipes(10000, PAGE_10);

        verify(devRecipePopularBudgetRepository).findBudgetDev(eq(10000), eq(List.of(1L, 2L, 3L)), any());
    }

    @Test
    @DisplayName("getBudgetRecipes(maxCost=null): Integer.MAX_VALUE로 변환 (운영 V2 fallback semantics)")
    void getBudgetRecipes_nullMaxCost_usesIntegerMaxValue() {
        given(devRecipePopularBudgetRepository.findTop10PopularDevIds(any())).willReturn(List.of(1L));
        given(devRecipePopularBudgetRepository.findBudgetDev(any(), any(), any())).willReturn(Page.empty(PAGE_10));

        service.getBudgetRecipes(null, PAGE_10);

        verify(devRecipePopularBudgetRepository).findBudgetDev(eq(Integer.MAX_VALUE), any(), any());
    }

    @Test
    @DisplayName("getBudgetRecipes(maxCost=-1): 음수도 Integer.MAX_VALUE로 변환 (방어 가드)")
    void getBudgetRecipes_negativeMaxCost_usesIntegerMaxValue() {
        given(devRecipePopularBudgetRepository.findTop10PopularDevIds(any())).willReturn(List.of(1L));
        given(devRecipePopularBudgetRepository.findBudgetDev(any(), any(), any())).willReturn(Page.empty(PAGE_10));

        service.getBudgetRecipes(-100, PAGE_10);

        verify(devRecipePopularBudgetRepository).findBudgetDev(eq(Integer.MAX_VALUE), any(), any());
    }

    @Test
    @DisplayName("getBudgetRecipes(top10 비어있음): sentinel -1L 사용 (NOT IN 절 SQL 에러 방지)")
    void getBudgetRecipes_emptyTop10_usesSentinelMinusOne() {
        given(devRecipePopularBudgetRepository.findTop10PopularDevIds(any())).willReturn(List.of()); // 빈 list
        given(devRecipePopularBudgetRepository.findBudgetDev(any(), any(), any())).willReturn(Page.empty(PAGE_10));

        service.getBudgetRecipes(10000, PAGE_10);

        // -1L sentinel — 운영 V2 동일 패턴, NOT IN (-1) 은 SQL 에러 안 남
        verify(devRecipePopularBudgetRepository).findBudgetDev(eq(10000), eq(List.of(-1L)), any());
    }

    @Test
    @DisplayName("getBudgetRecipes: 응답 DTO 타입이 DevRecipeSimpleStaticDtoV2 (cost 필드 포함하는 V2 변형)")
    void getBudgetRecipes_returnsV2DtoWithCost() {
        DevRecipeSimpleStaticDtoV2 dto = DevRecipeSimpleStaticDtoV2.builder()
                .id(1L).title("budget recipe")
                .ingredientCost(5000).marketPrice(8000)
                .build();
        given(devRecipePopularBudgetRepository.findTop10PopularDevIds(any())).willReturn(List.of(99L));
        given(devRecipePopularBudgetRepository.findBudgetDev(any(), any(), any()))
                .willReturn(new org.springframework.data.domain.PageImpl<>(List.of(dto), PAGE_10, 1));

        Page<DevRecipeSimpleStaticDtoV2> result = service.getBudgetRecipes(10000, PAGE_10);

        assertThat(result.getContent()).hasSize(1);
        assertThat(result.getContent().get(0).getIngredientCost()).isEqualTo(5000);
        assertThat(result.getContent().get(0).getMarketPrice()).isEqualTo(8000);
    }
}
