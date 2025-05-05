package com.jdc.recipe_service.opensearch.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
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
import java.util.*;
import java.util.stream.Collectors;

@Service
public class OpenSearchService {

    private final RestHighLevelClient client;
    private final RefrigeratorItemRepository fridgeRepo;


    public OpenSearchService(RestHighLevelClient client, RefrigeratorItemRepository fridgeRepo) {
        this.client = client;
        this.fridgeRepo = fridgeRepo;
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
            // 1) 사용자 냉장고 재료 ID 집합 조회
            Set<Long> fridgeIds = (userId != null)
                    ? fridgeRepo.findByUserId(userId, Pageable.unpaged())
                    .stream()
                    .map(item -> item.getIngredient().getId())
                    .collect(Collectors.toSet())
                    : Collections.emptySet();

            // 2) BoolQuery 구성 (prefix 검색 + 카테고리 필터)
            BoolQueryBuilder bool = QueryBuilders.boolQuery();
            if (q != null && !q.isBlank()) {
                bool.must(QueryBuilders.matchPhrasePrefixQuery("name", q));
            }
            if (categoryFilter != null && !categoryFilter.isBlank()) {
                bool.filter(QueryBuilders.termQuery("category.keyword", categoryFilter));
            }

            // 3) OpenSearch 에 전체 결과 요청 (size는 예상 최대치, 예: 전체 재료 수)
            SearchSourceBuilder src = new SearchSourceBuilder()
                    .query(bool)
                    .size(1_000);  // 재료가 많지 않다는 가정 하에

            // (필요 시 ES 단 정렬도 추가할 수 있으나, 아래 애플리케이션 레벨 정렬로 대체)
            SearchResponse resp = client.search(
                    new SearchRequest("ingredients").source(src),
                    RequestOptions.DEFAULT
            );

            // 4) DTO 변환 + 동적 inFridge 플래그 셋팅
            List<IngredientSummaryDto> all = Arrays.stream(resp.getHits().getHits())
                    .map(hit -> {
                        Map<String,Object> m = hit.getSourceAsMap();
                        Long id       = Long.valueOf(hit.getId());
                        String name   = Objects.toString(m.get("name"), "");
                        String category = Objects.toString(m.get("category"), "");
                        String imageUrl = Objects.toString(m.get("imageUrl"), "");
                        String unit   = Objects.toString(m.get("unit"), "");
                        boolean inFridge = fridgeIds.contains(id);

                        return new IngredientSummaryDto(
                                id, name, category, imageUrl, unit, inFridge
                        );
                    })
                    .collect(Collectors.toList());

            // 5) 애플리케이션 레벨 필터링 (inFridge)
            List<IngredientSummaryDto> filtered = (inFridgeFilter != null)
                    ? all.stream()
                    .filter(dto -> dto.inFridge() == inFridgeFilter)  // ← dto.isInFridge() → dto.inFridge()
                    .collect(Collectors.toList())
                    : all;

            // 6) 애플리케이션 레벨 정렬 (Pageable.getSort())
            if (pageable.getSort().isSorted()) {
                Comparator<IngredientSummaryDto> comp = null;
                for (Sort.Order order : pageable.getSort()) {
                    Comparator<IngredientSummaryDto> c;
                    switch (order.getProperty()) {
                        case "name":
                            c = Comparator.comparing(IngredientSummaryDto::name,
                                    String.CASE_INSENSITIVE_ORDER);
                            break;
                        case "category":
                            c = Comparator.comparing(IngredientSummaryDto::category,
                                    String.CASE_INSENSITIVE_ORDER);
                            break;
                        default:
                            continue;
                    }
                    if (!order.isAscending()) {
                        c = c.reversed();
                    }
                    comp = (comp == null) ? c : comp.thenComparing(c);
                }
                if (comp != null) {
                    filtered.sort(comp);
                }
            }

            // 7) 애플리케이션 레벨 페이징
            int total = filtered.size();
            int start = (int) pageable.getOffset();
            int end   = Math.min(start + pageable.getPageSize(), total);
            List<IngredientSummaryDto> pageList =
                    (start > total) ? Collections.emptyList() : filtered.subList(start, end);

            return new PageImpl<>(pageList, pageable, total);

        } catch (IOException e) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "재료 검색 중 오류: " + e.getMessage()
            );
        }
    }

}
