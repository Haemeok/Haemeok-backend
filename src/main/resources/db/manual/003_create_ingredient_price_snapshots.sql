CREATE TABLE IF NOT EXISTS ingredient_price_snapshots (
    id BIGINT NOT NULL AUTO_INCREMENT,
    ingredient_id BIGINT NOT NULL,
    source_name VARCHAR(50) NOT NULL,
    source_label VARCHAR(255) NULL,
    source_link TEXT NULL,
    price_per_g DECIMAL(12,4) NOT NULL,
    captured_at DATETIME NOT NULL,
    PRIMARY KEY (id),
    CONSTRAINT fk_ingredient_price_snapshots_ingredient
        FOREIGN KEY (ingredient_id) REFERENCES ingredients(id),
    INDEX idx_ingredient_price_snapshots_ingredient_captured_at (ingredient_id, captured_at),
    INDEX idx_ingredient_price_snapshots_source_name (source_name)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
