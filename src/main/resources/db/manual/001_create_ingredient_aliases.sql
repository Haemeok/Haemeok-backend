CREATE TABLE IF NOT EXISTS ingredient_aliases (
    id BIGINT NOT NULL AUTO_INCREMENT,
    ingredient_id BIGINT NOT NULL,
    alias_text VARCHAR(100) NOT NULL,
    normalized_alias VARCHAR(100) NOT NULL,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (id),
    CONSTRAINT fk_ingredient_aliases_ingredient
        FOREIGN KEY (ingredient_id) REFERENCES ingredients(id),
    CONSTRAINT uq_ingredient_aliases_ingredient_normalized
        UNIQUE (ingredient_id, normalized_alias),
    INDEX idx_ingredient_aliases_normalized_alias (normalized_alias)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
