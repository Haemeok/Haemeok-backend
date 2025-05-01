package com.jdc.recipe_service.opensearch.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.opensearch.action.search.SearchRequest;
import org.opensearch.action.search.SearchResponse;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.index.query.BoolQueryBuilder;
import org.opensearch.index.query.QueryBuilders;
import org.opensearch.search.builder.SearchSourceBuilder;
import org.opensearch.search.sort.SortOrder;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
public class OpenSearchService {

    private final RestHighLevelClient client;

    public OpenSearchService(RestHighLevelClient client) {
        this.client = client;
    }

    public Page<RecipeSimpleDto> searchRecipes(RecipeSearchCondition cond, Pageable pg, Long uid) {
        try {
            BoolQueryBuilder bool = QueryBuilders.boolQuery();
            if (cond.getTitle() != null)
                bool.must(QueryBuilders.multiMatchQuery(cond.getTitle(), "title","description","ingredients"));
            if (cond.getDishType() != null)
                bool.filter(QueryBuilders.termQuery("dishType.keyword", cond.getDishType()));
            if (cond.getTagNames() != null && !cond.getTagNames().isEmpty())
                bool.filter(QueryBuilders.termsQuery("tags.keyword", cond.getTagNames()));

            SearchSourceBuilder src = new SearchSourceBuilder()
                    .query(bool)
                    .from((int)pg.getOffset())
                    .size(pg.getPageSize());
            pg.getSort().forEach(o ->
                    src.sort(o.getProperty(), o.isAscending() ? SortOrder.ASC : SortOrder.DESC)
            );

            SearchResponse resp = client.search(
                    new SearchRequest("recipes").source(src),
                    RequestOptions.DEFAULT
            );

            List<RecipeSimpleDto> list = Arrays.stream(resp.getHits().getHits())
                    .map(h -> {
                        Map<String,Object> m = h.getSourceAsMap();
                        return new RecipeSimpleDto(
                                Long.valueOf(h.getId()),
                                (String)m.get("title"),
                                (String)m.get("imageUrl"),
                                (String)m.get("authorName"),
                                (String)m.get("profileImage"),
                                LocalDateTime.parse((String)m.get("createdAt")),
                                ((Number)m.getOrDefault("likeCount",0)).longValue(),
                                false,
                                new BigDecimal(m.getOrDefault("avgRating","0").toString()),
                                ((Number)m.getOrDefault("ratingCount",0)).longValue(),
                                ((Number)m.getOrDefault("cookingTime",0)).intValue()
                        );
                    }).collect(Collectors.toList());

            return new PageImpl<>(list, pg, resp.getHits().getTotalHits().value);
        } catch (IOException e) {
            throw new CustomException(ErrorCode.SEARCH_FAILURE, e.getMessage());
        }
    }

    public Page<IngredientSummaryDto> searchIngredients(
            String q,
            String categoryFilter,
            Boolean inFridgeFilter,
            Long userId,
            Pageable pageable
    ) {
        try {
            // 1) BoolQuery 구성
            BoolQueryBuilder bool = QueryBuilders.boolQuery();
            if (q != null)       bool.must(QueryBuilders.matchQuery("name", q));
            if (categoryFilter != null)
                bool.filter(QueryBuilders.termQuery("category.keyword", categoryFilter));
            if (inFridgeFilter != null)
                bool.filter(QueryBuilders.termQuery("inFridge", inFridgeFilter));

            // 2) SearchSourceBuilder
            SearchSourceBuilder src = new SearchSourceBuilder()
                    .query(bool)
                    .from((int) pageable.getOffset())
                    .size(pageable.getPageSize());
            // (필요 시 정렬 추가)

            // 3) 요청 실행
            SearchResponse resp = client.search(
                    new SearchRequest("ingredients").source(src),
                    RequestOptions.DEFAULT
            );

            // 4) DTO 변환 (5-arg record 생성자 호출)
            List<IngredientSummaryDto> list = Arrays.stream(resp.getHits().getHits())
                    .map(hit -> {
                        Map<String,Object> m = hit.getSourceAsMap();
                        Long    id        = Long.valueOf(hit.getId());
                        String  name      = (String) m.get("name");
                        String  category  = m.get("category") != null
                                ? m.get("category").toString()
                                : "";
                        String  imageUrl  = m.get("imageUrl") != null
                                ? m.get("imageUrl").toString()
                                : "";
                        String  unit      = m.get("unit") != null
                                ? m.get("unit").toString()
                                : "";
                        Object  fridgeVal = m.get("inFridge");
                        boolean inFridge  = fridgeVal instanceof Boolean
                                ? (Boolean) fridgeVal
                                : (fridgeVal instanceof Number
                                ? ((Number) fridgeVal).intValue() == 1
                                : false);

                        // IngredientSummaryDto record 의 생성자는
                        // (Long id, String name, String category, String imageUrl, boolean inFridge)
                        return new IngredientSummaryDto(
                                id,
                                name,
                                category,
                                imageUrl,
                                unit,
                                inFridge
                        );
                    })
                    .collect(Collectors.toList());

            // 5) 페이지 조립
            return new PageImpl<>(list, pageable, resp.getHits().getTotalHits().value);

        } catch (IOException e) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "재료 검색 중 오류: " + e.getMessage()
            );
        }
    }

}
