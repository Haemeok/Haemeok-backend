CREATE TABLE IF NOT EXISTS ingredient_candidates (
    id BIGINT NOT NULL AUTO_INCREMENT,
    candidate_type VARCHAR(20) NOT NULL,
    ingredient_id BIGINT NULL,
    raw_name VARCHAR(100) NULL,
    raw_unit_text VARCHAR(50) NULL,
    proposed_name_ko VARCHAR(100) NULL,
    proposed_unit_label_ko VARCHAR(50) NULL,
    proposed_grams_per_unit DECIMAL(12,3) NULL,
    proposed_edible_grams_per_unit DECIMAL(12,3) NULL,
    source_recipe_id BIGINT NULL,
    source_ref VARCHAR(255) NULL,
    status VARCHAR(20) NOT NULL,
    created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
    reviewed_at DATETIME NULL,
    PRIMARY KEY (id),
    CONSTRAINT fk_ingredient_candidates_ingredient
        FOREIGN KEY (ingredient_id) REFERENCES ingredients(id),
    CONSTRAINT fk_ingredient_candidates_recipe
        FOREIGN KEY (source_recipe_id) REFERENCES recipes(id),
    INDEX idx_ingredient_candidates_status_type (status, candidate_type),
    INDEX idx_ingredient_candidates_ingredient_id (ingredient_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
