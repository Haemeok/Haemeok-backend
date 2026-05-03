-- 큐레이션 아티클: 운영자가 직접 올리는 MDX 기반 매거진형 글.
-- 일반 유저 CRUD가 아니라 내부 운영 툴에서만 쓰는 어드민 전용 도메인이다.
-- 검색은 OpenSearch에 위임하므로 MySQL FULLTEXT 및 content_text 캐시는 두지 않는다.
-- v1에서는 다중 태그 대신 category 단일값으로 시작한다. 필요해지면 additive로 curation_article_tags를 추가한다.

-- =========================================================================
-- 1. curation_articles - 본문 (MDX 통째로 저장)
-- =========================================================================
CREATE TABLE curation_articles (
    id              BIGINT       NOT NULL AUTO_INCREMENT,
    slug            VARCHAR(255) NOT NULL,
    title           VARCHAR(255) NOT NULL,
    description     VARCHAR(500) NULL,
    cover_image_key VARCHAR(500) NULL,

    content_mdx     LONGTEXT     NOT NULL,

    category        VARCHAR(50)  NULL,
    -- DRAFT / PUBLISHED / ARCHIVED. Java enum + @Enumerated(STRING).
    status          VARCHAR(20)  NOT NULL DEFAULT 'DRAFT',

    generated_by    VARCHAR(100) NULL,
    human_reviewed  TINYINT(1)   NOT NULL DEFAULT 0,

    published_at    DATETIME(6)  NULL,
    created_at      DATETIME(6)  NOT NULL DEFAULT CURRENT_TIMESTAMP(6),
    updated_at      DATETIME(6)  NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),

    PRIMARY KEY (id),
    UNIQUE KEY uk_curation_articles_slug (slug),
    KEY idx_curation_articles_status_published   (status, published_at DESC, id DESC),
    KEY idx_curation_articles_category_published (category, status, published_at DESC, id DESC)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;


-- =========================================================================
-- 2. curation_article_recipe_refs - 아티클 ↔ 레시피 soft link (audit/reference)
-- =========================================================================
-- 화면 렌더링용 join 소스가 아니라, "이 아티클이 어떤 레시피를 참고해 만들어졌는지"
-- 추적용 테이블이다. 본문 자체는 self-contained MDX이므로 ref가 비어 있어도 글은 그대로 렌더된다.
--
-- recipe_id FK 미설정: 레시피 hard delete 후에도 아티클 생성 참고 이력을 보존한다.
-- chat_log처럼 audit/reference 성격의 느슨한 참조로 관리한다.
CREATE TABLE curation_article_recipe_refs (
    id         BIGINT      NOT NULL AUTO_INCREMENT,
    article_id BIGINT      NOT NULL,
    recipe_id  BIGINT      NOT NULL,

    created_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6),

    PRIMARY KEY (id),
    CONSTRAINT fk_curation_article_recipe_refs_article
        FOREIGN KEY (article_id) REFERENCES curation_articles (id) ON DELETE CASCADE,
    CONSTRAINT uk_curation_article_recipe_refs_article_recipe
        UNIQUE (article_id, recipe_id),
    KEY idx_curation_article_recipe_refs_article (article_id, id),
    KEY idx_curation_article_recipe_refs_recipe  (recipe_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
