-- 기존 ingredients 유지 + 새 구조 컬럼 추가
-- 레거시 컬럼(name, price, unit, calorie, carbohydrate, protein, fat, sugar, sodium 등)은 유지

  ALTER TABLE ingredients
      ADD COLUMN kcal_per_g DECIMAL(12,6) NULL,
      ADD COLUMN carbohydrate_g_per_g DECIMAL(12,6) NULL,
      ADD COLUMN protein_g_per_g DECIMAL(12,6) NULL,
      ADD COLUMN fat_g_per_g DECIMAL(12,6) NULL,
      ADD COLUMN sugar_g_per_g DECIMAL(12,6) NULL,
      ADD COLUMN sodium_mg_per_g DECIMAL(12,6) NULL,
      ADD COLUMN price_per_g DECIMAL(12,4) NULL,
      ADD COLUMN price_snapshot_id BIGINT NULL,
      ADD COLUMN is_active TINYINT(1) NOT NULL DEFAULT 1,
      ADD COLUMN created_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
      ADD COLUMN updated_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE
  CURRENT_TIMESTAMP;

ALTER TABLE ingredients
    ADD CONSTRAINT fk_ingredients_price_snapshot
        FOREIGN KEY (price_snapshot_id) REFERENCES ingredient_price_snapshots(id);
