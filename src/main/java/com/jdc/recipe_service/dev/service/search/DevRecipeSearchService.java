package com.jdc.recipe_service.dev.service.search;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDto;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleStaticDtoV2;
import com.jdc.recipe_service.dev.opensearch.service.DevRecipeQueryBuilder;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipePopularBudgetRepository;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeQueryRepositoryV2;
import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.util.SearchProperties;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.opensearch.action.search.SearchRequest;
import org.opensearch.action.search.SearchResponse;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.common.unit.TimeValue;
import org.opensearch.index.query.BoolQueryBuilder;
import org.opensearch.search.SearchHits;
import org.opensearch.search.builder.SearchSourceBuilder;
import org.opensearch.search.sort.SortBuilders;
import org.opensearch.search.sort.SortOrder;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.lang.Nullable;
import org.springframework.util.StringUtils;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.IOException;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Dev V3 search service.
 *
 * Engine 분기 정책:
 *  - {@code search.engine = querydsl} → 항상 QueryDSL
 *  - {@code search.engine = opensearch} → 항상 OpenSearch (dev alias)
 *  - {@code search.engine = auto} (기본) →
 *      - dev mirror disabled → QueryDSL (dev alias 비어있을 수 있음)
 *      - OpenSearch unhealthy → QueryDSL fallback
 *      - title 검색이 있을 때만 OpenSearch (운영 V2 동일 정책)
 *      - 그 외 → QueryDSL
 *
 * OpenSearch 호출 실패 시 QueryDSL로 자동 fallback (운영 V2와 동일).
 *
 * 운영 V2 {@code RecipeSearchServiceV2}와 분리. 운영 코드는 zero touch.
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class DevRecipeSearchService {

    private final RestHighLevelClient client;
    private final DevRecipeQueryBuilder queryBuilder;
    private final DevRecipeQueryRepositoryV2 devRecipeQueryRepository;
    private final DevRecipePopularBudgetRepository devRecipePopularBudgetRepository;
    private final SearchProperties searchProperties;

    @Value("${app.s3.bucket-name}")
    private String bucketName;

    @Value("${cloud.aws.region.static}")
    private String region;

    /** 운영 RecipeSearchServiceV2와 별개의 dev 전용 헬스 캐시 — 운영과 독립적으로 fallback 결정. */
    private volatile boolean isOpenSearchHealthy = true;

    @Transactional(readOnly = true)
    public Page<DevRecipeSimpleStaticDto> searchRecipes(RecipeSearchCondition condition,
                                                         Pageable pageable,
                                                         @Nullable Long viewerId) {
        if (shouldUseOpenSearch(condition)) {
            log.info("Dev V3 search: Using OpenSearch (alias={})", aliasName());
            try {
                return searchWithOpenSearch(condition, pageable, viewerId);
            } catch (Exception e) {
                log.warn("⚠️ Dev V3 OpenSearch 실패, QueryDSL fallback. error={}", e.getMessage());
                return devRecipeQueryRepository.searchStatic(condition, pageable, viewerId);
            }
        }
        log.info("Dev V3 search: Using QueryDSL");
        return devRecipeQueryRepository.searchStatic(condition, pageable, viewerId);
    }

    private Page<DevRecipeSimpleStaticDto> searchWithOpenSearch(RecipeSearchCondition cond,
                                                                 Pageable pg,
                                                                 @Nullable Long viewerId) throws IOException {
        BoolQueryBuilder bool = queryBuilder.buildSearchQuery(cond, viewerId);

        SearchSourceBuilder src = new SearchSourceBuilder()
                .query(bool)
                .from((int) pg.getOffset())
                .size(pg.getPageSize())
                .timeout(new TimeValue(3, TimeUnit.SECONDS))
                .fetchSource(false); // dev document 자체는 안 받음 — DB에서 정확한 DTO를 다시 조립

        if (cond.getTitle() != null && !cond.getTitle().isBlank()) {
            src.sort(SortBuilders.scoreSort().order(SortOrder.DESC));
        }
        if (pg.getSort().isSorted()) {
            pg.getSort().forEach(o -> src.sort(o.getProperty(),
                    o.isAscending() ? SortOrder.ASC : SortOrder.DESC));
        }

        SearchResponse resp = client.search(new SearchRequest(aliasName()).source(src), RequestOptions.DEFAULT);
        SearchHits hits = resp.getHits();

        List<Long> ids = Arrays.stream(hits.getHits())
                .map(h -> Long.valueOf(h.getId()))
                .collect(Collectors.toList());
        if (ids.isEmpty()) {
            return Page.empty(pg);
        }

        // OpenSearch hits 순서를 유지하며 DB DTO로 보강 (4 enum 포함).
        // viewerId를 전달해 DB 쪽에서 accessibleBy + imageReady 정책을 한 번 더 검증 — dev index stale 시 누수 차단.
        List<DevRecipeSimpleStaticDto> dtos = devRecipeQueryRepository.findAllByIds(ids, viewerId);
        Map<Long, DevRecipeSimpleStaticDto> dtoMap = dtos.stream()
                .collect(Collectors.toMap(DevRecipeSimpleStaticDto::getId, Function.identity()));

        List<DevRecipeSimpleStaticDto> ordered = ids.stream()
                .filter(dtoMap::containsKey)
                .map(dtoMap::get)
                .collect(Collectors.toList());

        // total 보정 + lag 가시화.
        // dev mirror가 비동기라 OpenSearch hit 중 일부가 DB accessibleBy/imageReady에서 차단될 수 있음.
        // 이 경우 같은 페이지의 dropped count만큼은 OS total에서 빼서 보정 (정확도 향상). 단, 다른 페이지에 남아 있는
        // stale hit은 보정 못 함 — totalElements가 일시적으로 과대 가능 (보안 사고 아님, 페이지네이션 부정확).
        // dev mirror lag가 일상적으로 큰 환경이면 stale 발견 시 QueryDSL fallback 정책 강화 검토.
        long osTotal = hits.getTotalHits().value;
        long staleHitsOnThisPage = (long) ids.size() - ordered.size();
        if (staleHitsOnThisPage > 0) {
            log.warn("[Dev V3 search] OpenSearch hit {}건이 DB 정책에서 차단됨 (dev index lag 가능성). " +
                            "page total 보정: {} → {}",
                    staleHitsOnThisPage, osTotal, osTotal - staleHitsOnThisPage);
        }
        long correctedTotal = Math.max(0, osTotal - staleHitsOnThisPage);

        return new PageImpl<>(ordered, pg, correctedTotal);
    }

    private boolean shouldUseOpenSearch(RecipeSearchCondition cond) {
        String engine = searchProperties.getEngine();

        // dev mirror가 비활성이면 dev alias가 비어있을 수 있으므로 OpenSearch 절대 사용 안 함
        SearchProperties.DevIndex devIndex = searchProperties.getDevIndex();
        boolean devIndexEnabled = devIndex != null && devIndex.isEnabled();
        if (!devIndexEnabled) {
            return false;
        }

        if ("opensearch".equalsIgnoreCase(engine)) return true;
        if ("querydsl".equalsIgnoreCase(engine)) return false;

        // auto: 운영 V2와 동일 — title 있을 때만 OpenSearch + healthy 조건
        if (this.isOpenSearchHealthy) {
            return cond.getTitle() != null && !cond.getTitle().isBlank();
        }
        return false;
    }

    private String aliasName() {
        return searchProperties.getDevIndex().getAlias();
    }

    // =========================================================================
    // popular / budget — 운영 V2 RecipeSearchServiceV2.getPopularRecipesStaticV2 / getBudgetRecipesStaticV2 미러
    // =========================================================================

    /**
     * Dev V3 인기 레시피.
     *  - weekly (default): {@code weeklyLikeCount + weeklyFavoriteCount} 컬럼 기반 fast path
     *  - monthly: 최근 30일 RecipeLike 실시간 COUNT
     *  - all (또는 그 외): 2000-01-01 이후 모든 RecipeLike COUNT
     *
     * 운영 V2의 {@code @Cacheable("popularRecipes")}는 dev에서 의도적으로 적용 안 함 — 검증 환경에서
     * stale 응답 사고 방지. dev 검색 빈도가 낮아 캐시 비용 부담 없음.
     */
    @Transactional(readOnly = true)
    public Page<DevRecipeSimpleStaticDto> getPopularRecipes(String period, Pageable pageable) {
        Page<DevRecipeSimpleStaticDto> page;
        if (!StringUtils.hasText(period) || "weekly".equalsIgnoreCase(period)) {
            log.info("Dev V3 popular: weekly column path");
            page = devRecipePopularBudgetRepository.findPopularDevWeekly(pageable);
        } else {
            log.info("Dev V3 popular: realtime count path (period={})", period);
            LocalDateTime startDate = calculateStartDate(period);
            page = devRecipePopularBudgetRepository.findPopularDevByRealtimeCount(startDate, pageable);
        }
        page.getContent().forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));
        return page;
    }

    /**
     * Dev V3 예산 레시피.
     * 운영 V2와 동일: top 10 인기 dev recipe 제외 + cost 범위 [1000, maxCost] + 가중 정렬.
     */
    @Transactional(readOnly = true)
    public Page<DevRecipeSimpleStaticDtoV2> getBudgetRecipes(Integer maxCost, Pageable pageable) {
        int effectiveMaxCost = (maxCost == null || maxCost < 0) ? Integer.MAX_VALUE : maxCost;

        // 운영 V2와 동일 패턴: 빈 list면 NOT IN 절 SQL 에러 → -1L sentinel
        List<Long> excludedIds = devRecipePopularBudgetRepository.findTop10PopularDevIds(PageRequest.of(0, 10));
        if (excludedIds.isEmpty()) {
            excludedIds = List.of(-1L);
        }

        Page<DevRecipeSimpleStaticDtoV2> page = devRecipePopularBudgetRepository.findBudgetDev(
                effectiveMaxCost, excludedIds, pageable);
        page.getContent().forEach(dto -> dto.setImageUrl(generateImageUrl(dto.getImageUrl())));
        return page;
    }

    /** 운영 V2 RecipeSearchServiceV2.calculateStartDate와 동일 시맨틱. */
    private LocalDateTime calculateStartDate(String period) {
        if ("monthly".equalsIgnoreCase(period)) {
            return LocalDateTime.now().minusDays(30);
        }
        return LocalDateTime.of(2000, 1, 1, 0, 0);
    }

    /** S3 image key → full URL. 운영 V2 RecipeSearchServiceV2.generateImageUrl과 동일 시맨틱. */
    private String generateImageUrl(String key) {
        if (key == null) return null;
        if (key.startsWith("http")) return key;
        return String.format("https://%s.s3.%s.amazonaws.com/%s", bucketName, region, key);
    }

    /** 운영 V2와 동일하게 10초 주기 헬스 캐시. dev mirror가 켜졌을 때만 실용적. */
    @Scheduled(initialDelay = 5000, fixedRate = 10000)
    public void checkOpenSearchHealth() {
        SearchProperties.DevIndex devIndex = searchProperties.getDevIndex();
        if (devIndex == null || !devIndex.isEnabled()) {
            // dev mirror off면 헬스 체크도 스킵 (운영 OS 부하 0)
            return;
        }
        boolean current;
        try {
            current = client.ping(RequestOptions.DEFAULT);
        } catch (Exception e) {
            current = false;
        }
        if (this.isOpenSearchHealthy != current) {
            log.info("Dev V3 OpenSearch health: {} -> {}", this.isOpenSearchHealthy, current);
            this.isOpenSearchHealthy = current;
        }
    }
}
