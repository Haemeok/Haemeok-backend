CREATE TABLE IF NOT EXISTS ingredient_units (
    id BIGINT NOT NULL AUTO_INCREMENT,
    ingredient_id BIGINT NOT NULL,
    unit_label_ko VARCHAR(50) NOT NULL,
    normalized_unit_label VARCHAR(50) NOT NULL,
    grams_per_unit DECIMAL(12,3) NOT NULL,
    edible_grams_per_unit DECIMAL(12,3) NOT NULL,
    is_default TINYINT(1) NOT NULL DEFAULT 0,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (id),
    CONSTRAINT fk_ingredient_units_ingredient
        FOREIGN KEY (ingredient_id) REFERENCES ingredients(id),
    CONSTRAINT uq_ingredient_units_ingredient_normalized
        UNIQUE (ingredient_id, normalized_unit_label),
    INDEX idx_ingredient_units_ingredient_id (ingredient_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
