package com.jdc.recipe_service.dev.opensearch.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.type.RecipeType;
import lombok.RequiredArgsConstructor;
import org.hashids.Hashids;
import org.opensearch.index.query.BoolQueryBuilder;
import org.opensearch.index.query.QueryBuilders;
import org.opensearch.index.query.RangeQueryBuilder;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * Dev V3 OpenSearch BoolQuery 빌더.
 *
 * 운영 {@link com.jdc.recipe_service.opensearch.service.OpenSearchService#buildCommonQuery}와 같은 검색 의미를
 * 갖되, 운영의 {@code bool.filter(termQuery("isPrivate", false))} 자리를
 * {@link DevRecipeSearchFilters#accessibleByFilter(Long)}로 대체한다 — RESTRICTED 누수 차단.
 *
 * 운영 코드는 zero touch (의존성 방향 깔끔, A1 dev/ 패키지 안에서 완결).
 *
 * 차이점:
 *  - 운영의 keywordService.record(keyword) 통계 기록은 호출 안 함 (dev 검색이 운영 키워드 통계에 섞이지 않게).
 */
@Component
@RequiredArgsConstructor
public class DevRecipeQueryBuilder {

    private final Hashids hashids;

    /**
     * 검색 빌더 — 검색은 discovery 단일 정책이라 viewer 분기 없이 publicListedActiveFilter만 적용한다.
     * owner의 자기 PRIVATE/UNLISTED는 검색에 섞이면 안 됨 (link-only 글도 직접 링크로만 접근).
     *
     * @param viewerId 시그니처 호환을 위해 유지하지만 검색 정책상 무시. 향후 cleanup 가능.
     */
    public BoolQueryBuilder buildSearchQuery(RecipeSearchCondition cond, @Nullable Long viewerId) {
        BoolQueryBuilder bool = QueryBuilders.boolQuery();

        // 검색은 discovery 단일 정책 — ACTIVE+PUBLIC+LISTED. viewer 무관.
        bool.filter(DevRecipeSearchFilters.publicListedActiveFilter());

        if (cond.getTitle() != null && !cond.getTitle().isBlank()) {
            String keyword = cond.getTitle().trim();
            BoolQueryBuilder keywordQuery = QueryBuilders.boolQuery();
            keywordQuery.should(QueryBuilders.termQuery("title.keyword", keyword).boost(10.0f));
            keywordQuery.should(QueryBuilders.matchQuery("title", keyword).boost(5.0f));
            keywordQuery.should(QueryBuilders.matchPhraseQuery("title", keyword).boost(3.0f));
            keywordQuery.should(QueryBuilders.matchPhrasePrefixQuery("title.prefix", keyword).boost(1.0f));
            keywordQuery.should(QueryBuilders.matchQuery("title.infix", keyword).boost(1.0f));
            keywordQuery.should(QueryBuilders.matchQuery("youtubeChannelName", keyword).boost(4.0f));
            keywordQuery.should(QueryBuilders.matchPhraseQuery("youtubeChannelName", keyword).boost(2.0f));
            bool.must(keywordQuery);
        }

        List<Long> ingredientIds = cond.getDecodedIngredientIds(hashids);
        if (!ingredientIds.isEmpty()) {
            for (Long ingId : ingredientIds) {
                bool.filter(QueryBuilders.termQuery("ingredientIds", ingId));
            }
        }

        applyRangeFilter(bool, "totalIngredientCost", cond.getMinCost(), cond.getMaxCost());
        applyRangeFilter(bool, "totalCalories", cond.getMinCalories(), cond.getMaxCalories());
        applyRangeFilter(bool, "protein", cond.getMinProtein(), cond.getMaxProtein());
        applyRangeFilter(bool, "carbohydrate", cond.getMinCarb(), cond.getMaxCarb());
        applyRangeFilter(bool, "fat", cond.getMinFat(), cond.getMaxFat());
        applyRangeFilter(bool, "sugar", cond.getMinSugar(), cond.getMaxSugar());
        applyRangeFilter(bool, "sodium", cond.getMinSodium(), cond.getMaxSodium());

        if (cond.getDishType() != null && !cond.getDishType().isBlank()) {
            bool.filter(QueryBuilders.termQuery("dishType", cond.getDishType()));
        }

        if (cond.getTags() != null && !cond.getTags().isEmpty()) {
            for (String tag : cond.getTags()) {
                bool.filter(QueryBuilders.termQuery("tags", tag));
            }
        }

        applyTypeFilter(bool, cond.getTypes());

        // bool 자체가 비면 match_all로 — accessibleByFilter는 항상 들어 있으므로 보통 trigger 안 됨
        if (bool.must().isEmpty() && bool.filter().isEmpty()) {
            bool.must(QueryBuilders.matchAllQuery());
        }
        return bool;
    }

    private void applyTypeFilter(BoolQueryBuilder mainBool, List<RecipeType> types) {
        if (types == null || types.isEmpty()) return;
        if (types.size() == RecipeType.values().length) return;

        BoolQueryBuilder typeQuery = QueryBuilders.boolQuery();
        for (RecipeType type : types) {
            switch (type) {
                case AI -> typeQuery.should(QueryBuilders.termQuery("isAiGenerated", true));
                case YOUTUBE -> typeQuery.should(QueryBuilders.boolQuery()
                        .must(QueryBuilders.existsQuery("youtubeUrl"))
                        .mustNot(QueryBuilders.termQuery("youtubeUrl", "")));
                case USER -> typeQuery.should(QueryBuilders.boolQuery()
                        .must(QueryBuilders.termQuery("isAiGenerated", false))
                        .must(QueryBuilders.boolQuery()
                                .should(QueryBuilders.boolQuery().mustNot(QueryBuilders.existsQuery("youtubeUrl")))
                                .should(QueryBuilders.termQuery("youtubeUrl", ""))
                        ));
            }
        }
        mainBool.filter(typeQuery);
    }

    private void applyRangeFilter(BoolQueryBuilder bool, String fieldName, Integer min, Integer max) {
        if (min == null && max == null) return;
        RangeQueryBuilder rangeQuery = QueryBuilders.rangeQuery(fieldName);
        rangeQuery.gte(min != null ? min : 0);
        if (max != null) rangeQuery.lte(max);
        bool.filter(rangeQuery);
    }
}
