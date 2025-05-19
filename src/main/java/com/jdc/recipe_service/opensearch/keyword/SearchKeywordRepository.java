package com.jdc.recipe_service.opensearch.keyword;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.List;
import java.util.Optional;

public interface SearchKeywordRepository extends JpaRepository<SearchKeyword, Long> {
    Optional<SearchKeyword> findByKeyword(String keyword);
    // prefix로 시작하는 인기 키워드, count 내림차순
    List<SearchKeyword> findByKeywordStartingWithIgnoreCaseOrderByCountDesc(
            String prefix, Pageable pageable);
}