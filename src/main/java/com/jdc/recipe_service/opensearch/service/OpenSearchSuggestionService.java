package com.jdc.recipe_service.opensearch.service;

import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.search.builder.SearchSourceBuilder;
import org.opensearch.search.suggest.Suggest;
import org.opensearch.search.suggest.SuggestBuilder;
import org.opensearch.search.suggest.completion.CompletionSuggestionBuilder;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class OpenSearchSuggestionService {

    private final RestHighLevelClient client;
    private final OpenSearchService  searchService;
    private static final String      INDEX = "ingredients";

    public OpenSearchSuggestionService(RestHighLevelClient client,
                                       OpenSearchService searchService) {
        this.client = client;
        this.searchService = searchService;
    }

    /**
     * 1) 문자열만 자동완성 제안
     * @param prefix    사용자 입력 접두어
     * @param category  카테고리 필터 (optional)
     * @param size      최대 제안 개수
     */
    @Cacheable(
            value = "ingSuggest",
            key   = "#prefix + ':' + (#category != null ? #category : '')",
            unless= "#result.isEmpty()"
    )
    public List<String> suggestIngredientNames(
            String prefix,
            String category,
            int size
    ) {
        try {
            CompletionSuggestionBuilder sugg = new CompletionSuggestionBuilder("name_suggest")
                    .prefix(prefix)
                    .size(size)
                    .skipDuplicates(true);
            SuggestBuilder sb = new SuggestBuilder().addSuggestion("ingredient-suggest", sugg);

            SearchSourceBuilder src = new SearchSourceBuilder().suggest(sb);

            var response = client.search(
                    new org.opensearch.action.search.SearchRequest(INDEX).source(src),
                    RequestOptions.DEFAULT
            );
            Suggest suggest = response.getSuggest();

            List<String> names = suggest.getSuggestion("ingredient-suggest")
                    .getEntries().stream()
                    .flatMap(entry -> entry.getOptions().stream())
                    .map(opt -> opt.getText().string())
                    .collect(Collectors.toList());

            if (category != null && !category.isBlank()) {
                var filtered = searchService
                        .searchIngredients(prefix, category, null, null,
                                PageRequest.of(0, Integer.MAX_VALUE))
                        .getContent().stream()
                        .map(IngredientSummaryDto::name)
                        .collect(Collectors.toSet());
                names = names.stream()
                        .filter(filtered::contains)
                        .limit(size)
                        .collect(Collectors.toList());
            }

            return names;
        } catch (IOException e) {
            throw new RuntimeException("자동완성 제안 실패: " + e.getMessage(), e);
        }
    }

    /**
     * 2) 풀 DTO 자동완성 제안 (이미지·단위·inFridge 포함)
     * @param prefix    사용자 입력 접두어
     * @param category  카테고리 필터 (optional)
     * @param inFridge  냉장고 필터 (optional)
     * @param size      최대 제안 개수
     * @param userId    로그인 사용자 ID
     */
    @Cacheable(
            value = "ingSuggestFull",
            key   = "#prefix + ':' + (#category != null ? #category : '') + ':' + (#inFridge != null ? #inFridge : '')",
            unless= "#result.isEmpty()"
    )
    public List<IngredientSummaryDto> suggestFull(
            String prefix,
            String category,
            Boolean inFridge,
            int size,
            Long userId
    ) {
        Pageable pg = PageRequest.of(0, size, Sort.by("name").ascending());
        return searchService
                .searchIngredients(prefix, category, inFridge, userId, pg)
                .getContent();
    }
}