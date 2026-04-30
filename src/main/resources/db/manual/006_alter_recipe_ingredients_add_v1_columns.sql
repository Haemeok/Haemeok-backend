-- 기존 recipe_ingredients 유지 + 새 정규화 컬럼 추가
-- quantity / unit / custom* 레거시 필드는 유지
-- 주의: 기존 UNIQUE(recipe_id, ingredient_id)는 이번 라운드에서 건드리지 않음

 ALTER TABLE recipe_ingredients
      ADD COLUMN ingredient_unit_id BIGINT NULL,
      ADD COLUMN ingredient_candidate_id BIGINT NULL,
      ADD COLUMN raw_name VARCHAR(100) NULL,
      ADD COLUMN raw_quantity_text VARCHAR(50) NULL,
      ADD COLUMN raw_unit_text VARCHAR(50) NULL,
      ADD COLUMN amount_value DECIMAL(12,3) NULL,
      ADD COLUMN normalized_grams DECIMAL(12,3) NULL,
      ADD COLUMN resolution_status VARCHAR(20) NULL;


  ALTER TABLE recipe_ingredients
      ADD CONSTRAINT fk_recipe_ingredients_unit
          FOREIGN KEY (ingredient_unit_id) REFERENCES ingredient_units(id),
      ADD CONSTRAINT fk_recipe_ingredients_candidate
          FOREIGN KEY (ingredient_candidate_id) REFERENCES ingredient_candidates(id);

  ALTER TABLE recipe_ingredients
      ADD INDEX idx_recipe_ingredients_unit_id (ingredient_unit_id),
      ADD INDEX idx_recipe_ingredients_candidate_id (ingredient_candidate_id);