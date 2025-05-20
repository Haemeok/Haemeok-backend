package com.jdc.recipe_service.opensearch.service;

import com.jdc.recipe_service.domain.dto.RecipeSearchCondition;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.opensearch.keyword.KeywordService;
import lombok.RequiredArgsConstructor;
import org.opensearch.action.search.SearchRequest;
import org.opensearch.action.search.SearchResponse;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.common.unit.Fuzziness;
import org.opensearch.index.query.BoolQueryBuilder;
import org.opensearch.index.query.QueryBuilders;
import org.opensearch.search.SearchHits;
import org.opensearch.search.SearchService;
import org.opensearch.search.builder.SearchSourceBuilder;
import org.opensearch.search.sort.SortOrder;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;


@Service
@RequiredArgsConstructor
public class OpenSearchService {

    private final RecipeRepository recipeRepository;
    private final RecipeLikeRepository recipeLikeRepository;
    private final RestHighLevelClient client;
    private final RefrigeratorItemRepository fridgeRepo;
    private final KeywordService keywordService;


    public Page<RecipeSimpleDto> searchRecipes(
            RecipeSearchCondition cond,
            Pageable pg,
            Long uid) {

        try {
            // 1) BoolQueryBuilder
            BoolQueryBuilder bool = QueryBuilders.boolQuery();

            // 1-1) Title: prefix + infix
            if (cond.getTitle() != null && !cond.getTitle().isBlank()) {
                String title = cond.getTitle().trim();
                keywordService.record(title);

                var titleQuery = QueryBuilders.boolQuery()
                        .should(QueryBuilders.matchPhrasePrefixQuery("title.prefix", title))
                        .should(QueryBuilders.matchQuery("title.infix", title));

                bool.must(titleQuery);
            }

            // 1-2) DishType filter
            if (cond.getDishType() != null && !cond.getDishType().isBlank()) {
                bool.filter(QueryBuilders.termQuery("dishType", cond.getDishType()));
            }

            // 1-3) Tags filter (AND)
            if (cond.getTagNames() != null && !cond.getTagNames().isEmpty()) {
                for (String tag : cond.getTagNames()) {
                    bool.filter(QueryBuilders.termQuery("tags", tag));
                }
            }

            // 1-4) match_all if no condition
            if (bool.must().isEmpty() && bool.filter().isEmpty()) {
                bool.must(QueryBuilders.matchAllQuery());
            }

            // 2) SearchSourceBuilder (paging + sort)
            var src = new SearchSourceBuilder()
                    .query(bool)
                    .from((int) pg.getOffset())
                    .size(pg.getPageSize());
            pg.getSort().forEach(o ->
                    src.sort(o.getProperty(),
                            o.isAscending() ? SortOrder.ASC : SortOrder.DESC)
            );

            // 3) Execute search
            SearchResponse resp = client.search(
                    new SearchRequest("recipes").source(src),
                    RequestOptions.DEFAULT
            );
            SearchHits hits = resp.getHits();

            // 4) Extract IDs
            List<Long> ids = Arrays.stream(hits.getHits())
                    .map(h -> Long.valueOf(h.getId()))
                    .collect(Collectors.toList());
            if (ids.isEmpty()) {
                return new PageImpl<>(Collections.emptyList(), pg, 0);
            }

            // 5) Load via JPA + map by ID
            List<Recipe> recipes = recipeRepository.findAllById(ids);
            Map<Long, Recipe> recipeMap = recipes.stream()
                    .collect(Collectors.toMap(Recipe::getId, Function.identity()));

            // 6) Check user likes
            Set<Long> likedIds = uid != null
                    ? recipeLikeRepository
                    .findByUserIdAndRecipeIdIn(uid, ids)
                    .stream()
                    .map(l -> l.getRecipe().getId())
                    .collect(Collectors.toSet())
                    : Collections.emptySet();

            // 7) Build DTO list in original order
            List<RecipeSimpleDto> list = ids.stream()
                    .filter(recipeMap::containsKey)
                    .map(id -> {
                        Recipe r = recipeMap.get(id);
                        return new RecipeSimpleDto(
                                r.getId(),
                                r.getTitle(),
                                r.getImageKey() == null ? null
                                        : String.format("https://%s.s3.%s.amazonaws.com/%s",
                                        "버킷명","리전",r.getImageKey()),
                                r.getUser().getNickname(),
                                r.getUser().getProfileImage(),
                                r.getCreatedAt(),
                                r.getLikes().size(),
                                likedIds.contains(r.getId()),
                                r.getAvgRating(),
                                r.getRatingCount(),
                                r.getCookingTime() == null ? 0 : r.getCookingTime()
                        );
                    })
                    .collect(Collectors.toList());

            return new PageImpl<>(list, pg, hits.getTotalHits().value);

        } catch (IOException e) {
            throw new CustomException(
                    ErrorCode.SEARCH_FAILURE,
                    "OpenSearch 조회 실패: " + e.getMessage()
            );
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
