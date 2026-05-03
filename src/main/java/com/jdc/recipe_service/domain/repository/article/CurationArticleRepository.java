package com.jdc.recipe_service.domain.repository.article;

import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface CurationArticleRepository extends JpaRepository<CurationArticle, Long> {

    boolean existsBySlug(String slug);

    /**
     * public 상세 조회용. PUBLISHED 외 status는 노출 금지이므로 status를 파라미터로 강제한다.
     * (admin은 raw findById를 쓴다 — 의도 차이를 메서드 시그니처로 분리)
     */
    Optional<CurationArticle> findBySlugAndStatus(String slug, ArticleStatus status);

    /**
     * 어드민 목록: status / category / q(title LIKE) 모두 optional. null이면 조건에서 제외.
     * Page로 반환해 어드민 화면 총 개수 노출을 지원한다.
     */
    @Query("""
            SELECT a FROM CurationArticle a
            WHERE (:status IS NULL OR a.status = :status)
              AND (:category IS NULL OR a.category = :category)
              AND (:q IS NULL OR a.title LIKE CONCAT('%', :q, '%'))
            """)
    Page<CurationArticle> search(@Param("status") ArticleStatus status,
                                 @Param("category") String category,
                                 @Param("q") String q,
                                 Pageable pageable);
}
