package com.jdc.recipe_service.opensearch.service;

import com.jdc.recipe_service.opensearch.keyword.SearchKeyword;
import com.jdc.recipe_service.opensearch.keyword.SearchKeywordRepository;
import lombok.RequiredArgsConstructor;
import org.opensearch.action.search.SearchRequest;
import org.opensearch.action.search.SearchResponse;
import org.opensearch.client.RequestOptions;
import org.opensearch.client.RestHighLevelClient;
import org.opensearch.index.query.QueryBuilders;
import org.opensearch.search.builder.SearchSourceBuilder;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Service
@RequiredArgsConstructor
public class OpenSearchSuggestionService {

    private final RestHighLevelClient client;
    private final SearchKeywordRepository keywordRepository;

    /**
     * 인기 키워드 + prefix-match 하이브리드 자동완성
     */
    public List<String> suggestRecipeTitles(String prefix, int size) {
        List<String> popular = keywordRepository
                .findByKeywordStartingWithIgnoreCaseOrderByCountDesc(
                        prefix, PageRequest.of(0, size))
                .stream()
                .map(SearchKeyword::getKeyword)
                .toList();

        if (popular.size() >= size) {
            return popular;
        }

        int remain = size - popular.size();
        try {
            SearchSourceBuilder src = new SearchSourceBuilder()
                    .query(QueryBuilders.matchPhrasePrefixQuery("title", prefix))
                    .size(remain)
                    .fetchSource(new String[]{"title"}, null);

            SearchResponse resp = client.search(
                    new SearchRequest("recipes").source(src),
                    RequestOptions.DEFAULT
            );

            List<String> docs = Arrays.stream(resp.getHits().getHits())
                    .map(h -> (String)h.getSourceAsMap().get("title"))
                    .distinct()
                    .toList();

            return Stream.concat(popular.stream(), docs.stream())
                    .collect(Collectors.toList());

        } catch (IOException e) {
            System.out.println("자동완성 제안 중 오류 발생: " + e.getMessage());
            return popular;
        }
    }

    /**
     * 전체 기간 동안 count 순으로 상위 size개 키워드를 반환
     */
    public List<String> getTopSearchKeywords(int size) {
        return keywordRepository
                .findAll(PageRequest.of(0, size, Sort.by("count").descending()))
                .stream()
                .map(SearchKeyword::getKeyword)
                .toList();
    }


}