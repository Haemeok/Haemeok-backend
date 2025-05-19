package com.jdc.recipe_service.opensearch.keyword;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
public class KeywordService {

    private final SearchKeywordRepository repo;

    @Transactional
    public void record(String kw) {
        repo.findByKeyword(kw)
                .ifPresentOrElse(
                        SearchKeyword::increment,
                        () -> repo.save(
                                SearchKeyword.builder()
                                        .keyword(kw)
                                        .count(1)
                                        .build()
                        )
                );
    }
}