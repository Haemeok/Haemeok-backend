-- 레시피북(폴더) 기능: 즐겨찾기와 독립적인 폴더 기반 레시피 정리 시스템
-- 기존 recipe_favorites 테이블에는 변경 없음 (additive only)

-- 유저당 기본 폴더 1개 보장: functional unique index 사용.
-- is_default=1인 행만 user_id로 유니크 제약을 걸고, 0인 행은 NULL이므로 무시된다.
-- (MySQL 8.0.13+ functional index, STORED generated column + UNIQUE 조합은 RDS에서 거부될 수 있어 대체)

CREATE TABLE recipe_books (
    id              BIGINT       NOT NULL AUTO_INCREMENT,
    user_id         BIGINT       NOT NULL,
    name            VARCHAR(50)  NOT NULL,
    is_default      TINYINT(1)   NOT NULL DEFAULT 0,
    display_order   INT          NOT NULL DEFAULT 0,
    recipe_count    INT          NOT NULL DEFAULT 0,
    created_at      DATETIME(6)  NOT NULL,
    updated_at      DATETIME(6)  NOT NULL,
    PRIMARY KEY (id),
    CONSTRAINT fk_recipe_books_user
        FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE,
    INDEX idx_recipe_books_user_order (user_id, display_order)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- 유저당 기본 폴더 1개 제약: functional unique index
CREATE UNIQUE INDEX uk_recipe_books_one_default_per_user
    ON recipe_books ((CASE WHEN is_default = 1 THEN user_id END));

-- 레시피북-레시피 매핑 테이블

CREATE TABLE recipe_book_items (
    id         BIGINT      NOT NULL AUTO_INCREMENT,
    book_id    BIGINT      NOT NULL,
    recipe_id  BIGINT      NOT NULL,
    created_at DATETIME(6) NOT NULL,
    updated_at DATETIME(6) NOT NULL,
    PRIMARY KEY (id),
    CONSTRAINT fk_recipe_book_items_book
        FOREIGN KEY (book_id) REFERENCES recipe_books (id) ON DELETE CASCADE,
    CONSTRAINT fk_recipe_book_items_recipe
        FOREIGN KEY (recipe_id) REFERENCES recipes (id) ON DELETE CASCADE,
    CONSTRAINT uk_recipe_book_items_book_recipe
        UNIQUE (book_id, recipe_id),
    INDEX idx_recipe_book_items_book_created (book_id, created_at DESC)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
