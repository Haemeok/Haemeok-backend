package com.jdc.recipe_service.repository.article;

import com.jdc.recipe_service.config.JpaAuditingConfig;
import com.jdc.recipe_service.config.QuerydslConfig;
import com.jdc.recipe_service.domain.entity.article.CurationArticle;
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
