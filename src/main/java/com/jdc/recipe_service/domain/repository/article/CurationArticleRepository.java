package com.jdc.recipe_service.domain.repository.article;

import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.projection.article.CurationArticleRecommendationProjection;
import com.jdc.recipe_service.domain.projection.article.CurationArticleSitemapProjection;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
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

    /**
     * sitemap 생성용 projection. PUBLISHED 만 반환하며 정렬은 updatedAt DESC, id DESC로
     * 강제한다 — 클라이언트가 sort 파라미터를 변조해도 운영 의도와 다른 결과가 나오지 않게,
     * status 필터와 정렬을 모두 query에서 박는다.
     */
    @Query("""
            SELECT a.slug AS slug, a.updatedAt AS updatedAt
            FROM CurationArticle a
            WHERE a.status = com.jdc.recipe_service.domain.type.article.ArticleStatus.PUBLISHED
            ORDER BY a.updatedAt DESC, a.id DESC
            """)
    List<CurationArticleSitemapProjection> findAllForSitemap();

    /**
     * 추천 후보(같은 카테고리). PUBLISHED + 같은 category + 현재 article 제외.
     *
     * <p>전체 후보를 반환한다 — Pageable로 잘라내면 자른 부분집합 안에서만 deterministic random이 돌아
     * "전체 PUBLISHED가 후보"라는 정책과 어긋난다. 응답 컬럼이 5개뿐(id/slug/title/coverImageKey/category)이라
     * V1 운영 규모에선 부담이 작다. 아티클 수가 수만 단위로 늘어 메모리/페이로드가 문제되면 그때 다시 검토.
     *
     * <p>ORDER BY는 두지 않는다 — service의 deterministic random sort가 단일 source-of-truth.
     */
    @Query("""
            SELECT a.id AS id, a.slug AS slug, a.title AS title,
                   a.coverImageKey AS coverImageKey, a.category AS category
            FROM CurationArticle a
            WHERE a.status = com.jdc.recipe_service.domain.type.article.ArticleStatus.PUBLISHED
              AND a.category = :category
              AND a.id <> :excludeId
            """)
    List<CurationArticleRecommendationProjection> findRecommendationCandidatesByCategory(
            @Param("category") String category,
            @Param("excludeId") Long excludeId);

    /**
     * 추천 후보(explore — category 무제한). PUBLISHED + 현재 article 제외.
     *
     * <p>sameCategory에서 이미 선택된 id는 service에서 in-memory dedup으로 거른다 (post-fetch filter가 단순함).
     * 후보 상한 정책에 대한 설명은 {@link #findRecommendationCandidatesByCategory}와 동일.
     */
    @Query("""
            SELECT a.id AS id, a.slug AS slug, a.title AS title,
                   a.coverImageKey AS coverImageKey, a.category AS category
            FROM CurationArticle a
            WHERE a.status = com.jdc.recipe_service.domain.type.article.ArticleStatus.PUBLISHED
              AND a.id <> :excludeId
            """)
    List<CurationArticleRecommendationProjection> findRecommendationExploreCandidates(
            @Param("excludeId") Long excludeId);
}
