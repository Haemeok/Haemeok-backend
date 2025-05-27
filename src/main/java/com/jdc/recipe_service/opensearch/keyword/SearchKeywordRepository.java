package com.jdc.recipe_service.opensearch.keyword;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface SearchKeywordRepository extends JpaRepository<SearchKeyword, Long> {
    Optional<SearchKeyword> findByKeyword(String keyword);

    List<SearchKeyword> findByKeywordStartingWithIgnoreCaseOrderByCountDesc(
            String prefix, Pageable pageable);
}