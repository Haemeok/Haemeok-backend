package com.jdc.recipe_service.repository.article;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
import com.jdc.recipe_service.domain.projection.article.CurationArticleRecommendationProjection;
import com.jdc.recipe_service.domain.projection.article.CurationArticleSitemapProjection;
import com.jdc.recipe_service.domain.repository.article.CurationArticleRepository;
import com.jdc.recipe_service.domain.type.article.ArticleStatus;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.jdbc.AutoConfigureTestDatabase;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.context.annotation.Import;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.context.TestPropertySource;
import org.testcontainers.containers.MySQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.time.LocalDateTime;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@DataJpaTest
@AutoConfigureTestDatabase(replace = AutoConfigureTestDatabase.Replace.NONE)
@Testcontainers
@Import({QuerydslConfig.class, JpaAuditingConfig.class})
@TestPropertySource(properties = {
        "spring.flyway.enabled=false",
        "spring.jpa.hibernate.ddl-auto=create-drop",
        "app.s3.bucket-name=test-bucket",
        "cloud.aws.region.static=ap-northeast-2"
})
class CurationArticleRepositoryTest {

    @Container
    @ServiceConnection
    static MySQLContainer<?> mysql = new MySQLContainer<>("mysql:8.0.33");

    @Autowired private EntityManager em;
    @Autowired private CurationArticleRepository articleRepo;

    @Test
    @DisplayName("동일 slug 두 번 저장 시 unique 제약으로 실패한다")
    void slugIsUnique() {
        articleRepo.save(buildArticle("dup-slug", "첫 글", ArticleStatus.DRAFT, "diet"));
        em.flush();

        assertThatThrownBy(() -> {
                    articleRepo.save(buildArticle("dup-slug", "두번째 글", ArticleStatus.DRAFT, "diet"));
                    em.flush();
                })
                .isInstanceOf(DataIntegrityViolationException.class);
    }

    @Test
    @DisplayName("status + category + q(title LIKE) 모두 적용된 검색이 일치하는 행만 반환한다")
    void searchAppliesAllFilters() {
        articleRepo.save(buildArticle("a-1", "여름 다이어트 한식", ArticleStatus.PUBLISHED, "diet"));
        articleRepo.save(buildArticle("a-2", "겨울 양식 추천",   ArticleStatus.PUBLISHED, "winter"));
        articleRepo.save(buildArticle("a-3", "여름 다이어트 양식", ArticleStatus.DRAFT,     "diet"));
        em.flush();
        em.clear();

        Page<CurationArticle> page = articleRepo.search(
                ArticleStatus.PUBLISHED,
                "diet",
                "여름",
                PageRequest.of(0, 20)
        );

        assertThat(page.getTotalElements()).isEqualTo(1L);
        assertThat(page.getContent())
                .extracting(CurationArticle::getSlug)
                .containsExactly("a-1");
    }

    @Test
    @DisplayName("필터가 모두 null이면 전체가 반환된다 (Page 총 개수 포함)")
    void searchWithAllNullFiltersReturnsAll() {
        articleRepo.save(buildArticle("b-1", "글 A", ArticleStatus.DRAFT, null));
        articleRepo.save(buildArticle("b-2", "글 B", ArticleStatus.PUBLISHED, "diet"));
        articleRepo.save(buildArticle("b-3", "글 C", ArticleStatus.ARCHIVED, "winter"));
        em.flush();
        em.clear();

        Page<CurationArticle> page = articleRepo.search(null, null, null, PageRequest.of(0, 20));

        assertThat(page.getTotalElements()).isEqualTo(3L);
    }

    @Test
    @DisplayName("existsBySlug는 등록된 slug에만 true를 반환한다")
    void existsBySlug() {
        articleRepo.save(buildArticle("exists-slug", "t", ArticleStatus.DRAFT, null));
        em.flush();

        assertThat(articleRepo.existsBySlug("exists-slug")).isTrue();
        assertThat(articleRepo.existsBySlug("missing-slug")).isFalse();
    }

    @Test
    @DisplayName("findAllForSitemap은 PUBLISHED만 반환하며 정렬은 updatedAt DESC, id DESC다 (DRAFT/ARCHIVED 제외)")
    void findAllForSitemap() {
        LocalDateTime t1 = LocalDateTime.of(2026, 5, 1, 10, 0);
        LocalDateTime t2 = LocalDateTime.of(2026, 5, 2, 10, 0);
        LocalDateTime t3 = LocalDateTime.of(2026, 5, 3, 10, 0);

        CurationArticle pubOlder  = articleRepo.save(buildArticle("pub-old",   "옛날 글",   ArticleStatus.PUBLISHED, "diet"));
        CurationArticle pubMid1   = articleRepo.save(buildArticle("pub-mid-1", "동률 글 1", ArticleStatus.PUBLISHED, "diet"));
        CurationArticle pubMid2   = articleRepo.save(buildArticle("pub-mid-2", "동률 글 2", ArticleStatus.PUBLISHED, "diet"));
        CurationArticle pubNewest = articleRepo.save(buildArticle("pub-new",   "최신 글",   ArticleStatus.PUBLISHED, "diet"));
        CurationArticle draftOne  = articleRepo.save(buildArticle("draft",    "초안",     ArticleStatus.DRAFT,     "diet"));
        CurationArticle archived  = articleRepo.save(buildArticle("arch",     "아카이브",   ArticleStatus.ARCHIVED,  "diet"));
        em.flush();

        forceUpdatedAt(pubOlder.getId(),  t1);
        forceUpdatedAt(pubMid1.getId(),   t2);
        forceUpdatedAt(pubMid2.getId(),   t2);
        forceUpdatedAt(pubNewest.getId(), t3);
        forceUpdatedAt(draftOne.getId(),  t3);
        forceUpdatedAt(archived.getId(),  t3);
        em.clear();

        List<CurationArticleSitemapProjection> result = articleRepo.findAllForSitemap();

        assertThat(result).extracting(CurationArticleSitemapProjection::getSlug)
                .doesNotContain("draft", "arch");
        assertThat(result).hasSize(4);

        assertThat(result).extracting(CurationArticleSitemapProjection::getSlug)
                .containsExactly("pub-new", "pub-mid-2", "pub-mid-1", "pub-old");
    }

    @Test
    @DisplayName("findRecommendationCandidatesByCategory: PUBLISHED + 같은 category + current 제외만 반환 (DRAFT/ARCHIVED/다른 category 제외)")
    void findRecommendationCandidatesByCategory_filtersCorrectly() {
        CurationArticle current  = articleRepo.save(buildArticle("current",   "현재 글",     ArticleStatus.PUBLISHED, "diet"));
        CurationArticle dietA    = articleRepo.save(buildArticle("diet-a",    "다이어트 A",  ArticleStatus.PUBLISHED, "diet"));
        CurationArticle dietB    = articleRepo.save(buildArticle("diet-b",    "다이어트 B",  ArticleStatus.PUBLISHED, "diet"));
        CurationArticle winterC  = articleRepo.save(buildArticle("winter-c",  "겨울 C",      ArticleStatus.PUBLISHED, "winter"));
        CurationArticle dietDraft = articleRepo.save(buildArticle("diet-draft", "초안 다이어트", ArticleStatus.DRAFT,    "diet"));
        CurationArticle dietArch  = articleRepo.save(buildArticle("diet-arch",  "보관 다이어트", ArticleStatus.ARCHIVED, "diet"));
        em.flush();
        em.clear();

        List<CurationArticleRecommendationProjection> result = articleRepo.findRecommendationCandidatesByCategory(
                "diet", current.getId());

        assertThat(result).extracting(CurationArticleRecommendationProjection::getSlug)
                .containsExactlyInAnyOrder("diet-a", "diet-b")
                .doesNotContain("current", "winter-c", "diet-draft", "diet-arch");
    }

    @Test
    @DisplayName("findRecommendationExploreCandidates: PUBLISHED + current 제외, category 무제한 (DRAFT/ARCHIVED 제외)")
    void findRecommendationExploreCandidates_returnsAllPublishedExceptCurrent() {
        CurationArticle current  = articleRepo.save(buildArticle("current",  "현재",   ArticleStatus.PUBLISHED, "diet"));
        CurationArticle pubA     = articleRepo.save(buildArticle("pub-a",    "글 A",   ArticleStatus.PUBLISHED, "diet"));
        CurationArticle pubB     = articleRepo.save(buildArticle("pub-b",    "글 B",   ArticleStatus.PUBLISHED, "winter"));
        CurationArticle pubC     = articleRepo.save(buildArticle("pub-c",    "글 C",   ArticleStatus.PUBLISHED, null));
        CurationArticle draft    = articleRepo.save(buildArticle("draft",    "초안",   ArticleStatus.DRAFT,     "spring"));
        CurationArticle archived = articleRepo.save(buildArticle("archived", "보관",   ArticleStatus.ARCHIVED,  "summer"));
        em.flush();
        em.clear();

        List<CurationArticleRecommendationProjection> result = articleRepo.findRecommendationExploreCandidates(
                current.getId());

        assertThat(result).extracting(CurationArticleRecommendationProjection::getSlug)
                .containsExactlyInAnyOrder("pub-a", "pub-b", "pub-c")
                .doesNotContain("current", "draft", "archived");
    }

    @Test
    @DisplayName("findRecommendationCandidates*: 후보 상한 없이 PUBLISHED 전체를 반환한다 (정책: 전체 후보 풀)")
    void recommendationQueries_returnAllPublishedCandidates() {
        CurationArticle current = articleRepo.save(buildArticle("current", "현재", ArticleStatus.PUBLISHED, "diet"));
        for (int i = 0; i < 8; i++) {
            articleRepo.save(buildArticle("diet-" + i, "글 " + i, ArticleStatus.PUBLISHED, "diet"));
        }
        em.flush();
        em.clear();

        List<CurationArticleRecommendationProjection> sameCat = articleRepo.findRecommendationCandidatesByCategory(
                "diet", current.getId());
        List<CurationArticleRecommendationProjection> explore = articleRepo.findRecommendationExploreCandidates(
                current.getId());

        // current 제외하고 PUBLISHED 8개가 모두 후보로 들어와야 한다 — Pageable cap 없이 전체 풀.
        assertThat(sameCat).hasSize(8);
        assertThat(explore).hasSize(8);
    }

    private void forceUpdatedAt(Long id, LocalDateTime t) {
        em.createQuery("UPDATE CurationArticle a SET a.updatedAt = :t WHERE a.id = :id")
                .setParameter("t", t)
                .setParameter("id", id)
                .executeUpdate();
    }

    private CurationArticle buildArticle(String slug, String title, ArticleStatus status, String category) {
        return CurationArticle.builder()
                .slug(slug)
                .title(title)
                .contentMdx("# body")
                .category(category)
                .status(status)
                .humanReviewed(false)
                .build();
    }
}
