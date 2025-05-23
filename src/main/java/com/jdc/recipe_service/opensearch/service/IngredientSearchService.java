package com.jdc.recipe_service.opensearch.service;

import com.jdc.recipe_service.opensearch.dto.IngredientSearchDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.opensearch.action.search.SearchRequest;
import org.opensearch.action.search.SearchResponse;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.index.query.BoolQueryBuilder;
import org.opensearch.index.query.QueryBuilders;
import org.opensearch.search.SearchHit;
import org.opensearch.search.builder.SearchSourceBuilder;
import org.opensearch.search.sort.SortOrder;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.*;
import org.springframework.stereotype.Service;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class IngredientSearchService {

    private final RestHighLevelClient client;
    private final IngredientRepository ingredientRepo;

    private static final String INDEX = "ingredients";

    private IngredientSearchDto mapToDto(SearchHit hit) {
        Map<String,Object> m = hit.getSourceAsMap();
        Long id   = Long.valueOf(hit.getId());        // ← 여기서 문서 ID를 꺼내고
        String nm = (String) m.get("name");
        String ct = (String) m.get("category");
        String url = String.format(
                "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/ingredients/%s.webp",
                nm
        );
        return new IngredientSearchDto(id, nm, ct, url);
    }

    /**
     * OpenSearch 색인 + 캐시 적용 검색
     * 실패 시 JPA 조회로 대체
     */
    @Cacheable(
            value = "ingredientSearch",
            key = "#q + '::' + #category + '::' +#pageable.pageNumber + ':' + #pageable.pageSize + ':' + #pageable.sort.toString()"
    )
    public Page<IngredientSearchDto> search(
            String q,
            String category,
            Pageable pageable
    ) {
        // OpenSearch용 BoolQuery 조립
        BoolQueryBuilder bool = QueryBuilders.boolQuery();
        if (q != null && !q.isBlank()) {
            bool.must(QueryBuilders.matchPhrasePrefixQuery("name", q));
        } else {
            bool.must(QueryBuilders.matchAllQuery());
        }
        if (category != null && !category.isBlank()) {
            bool.filter(QueryBuilders.termQuery("category.keyword", category));
        }

        // 페이징·정렬 설정
        SearchSourceBuilder src = new SearchSourceBuilder()
                .query(bool)
                .from((int) pageable.getOffset())
                .size(pageable.getPageSize());
        pageable.getSort().forEach(o ->
                src.sort(o.getProperty(),
                        o.isAscending() ? SortOrder.ASC : SortOrder.DESC)
        );

        try {
            // OpenSearch 실행
            SearchResponse resp = client.search(
                    new SearchRequest(INDEX).source(src),
                    RequestOptions.DEFAULT
            );
            long total = resp.getHits().getTotalHits().value;
            List<IngredientSearchDto> list = List.of(resp.getHits().getHits())
                    .stream()
                    .map(this::mapToDto)
                    .collect(Collectors.toList());

            return new PageImpl<>(list, pageable, total);

        } catch (Exception e) {
            log.warn("OpenSearch 검색 실패: {} (페일백 시도)", e.getMessage());
            try {
                return fallbackSearch(q, category, pageable);
            } catch (Exception ex) {
                throw new CustomException(
                        ErrorCode.INGREDIENT_FALLBACK_SEARCH_ERROR,
                        "재료 대체 검색 처리 중 오류: " + ex.getMessage()
                );
            }
        }
    }

    /** OpenSearch 실패 시 JPA 조회로 대체 */
    private Page<IngredientSearchDto> fallbackSearch(
            String q,
            String category,
            Pageable pageable
    ) {
        Page<Ingredient> page = ingredientRepo.findAll((root, cq, cb) -> {
            var preds = cb.conjunction();
            if (q != null && !q.isBlank()) {
                preds.getExpressions().add(
                        cb.like(cb.lower(root.get("name")), "%" + q.toLowerCase() + "%")
                );
            }
            if (category != null && !category.isBlank()) {
                preds.getExpressions().add(
                        cb.equal(root.get("category"), category)
                );
            }
            return preds;
        }, pageable);

        List<IngredientSearchDto> list = page.stream()
                .map(ing -> {
                    String url = String.format(
                            "https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/ingredients/%s.webp",
                            ing.getName()
                    );
                    return new IngredientSearchDto(
                            ing.getId(), ing.getName(), ing.getCategory(), url
                    );
                })
                .collect(Collectors.toList());

        return new PageImpl<>(list, pageable, page.getTotalElements());
    }
}