package com.jdc.recipe_service.opensearch.keyword;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@RequiredArgsConstructor
@Slf4j
public class KeywordService {

    private final SearchKeywordRepository repo;

    /**
     * 비동기로 실행되어 메인 검색 로직을 블로킹하지 않음.
     * 검색어 통계 집계가 조금 늦거나 실패해도, 사용자 검색 결과에는 영향 없음.
     */
    @Async
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void record(String kw) {
        try {
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
        } catch (Exception e) {
            log.warn("검색어 집계 실패 (keyword={}): {}", kw, e.getMessage());
        }
    }
}