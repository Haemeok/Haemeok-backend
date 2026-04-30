-- ===========================================================================
-- 2026-04-30 recipe_ingredients 2차 백필 전 missing unit 상위 30 검수 결과
-- ===========================================================================
-- 목적:
--   - dump20260430 기준 recipe_ingredients에서 많이 막히는 unit 중,
--     기존 ingredient_units 값 또는 표준 계량으로 안전하게 보강 가능한 항목만 추가한다.
--   - 이 SQL은 수동 실행 전용이며 Flyway 자동 실행 대상이 아니다.
--
-- 실행 위치:
--   - 001~006 스키마 확장 완료 후
--   - workbench/ingredient-research-pipeline/data/results/sql-full/_run_all.sql seed 완료 후
--   - Java normalization backfill 실행 전
--
-- 정책:
--   - add-only: 기존 row 삭제/변경 없음
--   - UNIQUE(ingredient_id, normalized_unit_label)에 대해 idempotent
--   - ingredient id/name이 맞는 경우에만 insert
--   - 새로 추가하는 보강 단위는 is_default=0
--
-- 상위 30 중 보류한 항목:
--   - 빵/개: 개당 크기 변동이 큼
--   - 모짜렐라치즈/봉지, 모짜렐라치즈/개: 포장/형태별 중량 변동이 큼
--   - 다진마늘/개: "쪽" 의미인지 포장 "개" 의미인지 불명확
--   - 브리치즈/개: 제품 크기 변동이 큼
--   - 코인 육수/ml: 재료 성격과 단위가 맞지 않아 원문 검수 필요
--   - 바게트/개: 제품 크기 변동이 큼
--   - 치킨스톡/개: 큐브/포/스틱 등 형태별 중량 변동이 큼
-- ===========================================================================

START TRANSACTION;

INSERT INTO ingredient_units (
  ingredient_id,
  normalized_unit_label,
  unit_label_ko,
  grams_per_unit,
  edible_grams_per_unit,
  is_default,
  created_at
)
SELECT
  s.ingredient_id,
  s.normalized_unit_label,
  s.unit_label_ko,
  s.grams_per_unit,
  s.edible_grams_per_unit,
  0,
  NOW()
FROM (
  SELECT 167 AS ingredient_id, '밥' AS ingredient_name, '공기' AS normalized_unit_label, '공기' AS unit_label_ko, 220.000 AS grams_per_unit, 220.000 AS edible_grams_per_unit
  UNION ALL SELECT 98, '두부', '모', '모', 300.000, 300.000
  UNION ALL SELECT 321, '즉석밥', '개', '개', 210.000, 210.000
  UNION ALL SELECT 110, '라면', '개', '개', 120.000, 120.000
  UNION ALL SELECT 420, '흑설탕', '큰술', '큰술', 12.500, 12.500
  UNION ALL SELECT 61, '다시팩', '개', '개', 10.000, 10.000
  UNION ALL SELECT 278, '연두부', '모', '모', 300.000, 300.000
  UNION ALL SELECT 324, '짜장라면', '개', '개', 140.000, 140.000
  UNION ALL SELECT 588, '아가베시럽', 'ml', 'ml', 1.400, 1.400
  UNION ALL SELECT 380, '튀김가루', 'ml', 'ml', 0.500, 0.500
  UNION ALL SELECT 353, '치킨스톡', 'ml', 'ml', 1.000, 1.000
  UNION ALL SELECT 353, '치킨스톡', '컵', '컵', 240.000, 240.000
  UNION ALL SELECT 585, '식용금박', 'g', 'g', 1.000, 1.000
  UNION ALL SELECT 241, '슬라이스치즈', '개', '개', 20.000, 20.000
  UNION ALL SELECT 88, '돼지 등뼈', 'kg', 'kg', 1000.000, 400.000
  UNION ALL SELECT 157, '밀가루', 'ml', 'ml', 0.500, 0.500
  UNION ALL SELECT 192, '블랙올리브', '알', '알', 4.500, 4.500
  UNION ALL SELECT 477, '돼지 등갈비', 'kg', 'kg', 1000.000, 550.000
  UNION ALL SELECT 601, '에멘탈치즈', '장', '장', 10.000, 10.000
  UNION ALL SELECT 78, '대파', '개', '개', 120.000, 120.000
  UNION ALL SELECT 353, '치킨스톡', 'l', 'L', 1000.000, 1000.000
  UNION ALL SELECT 314, '전분', 'ml', 'ml', 0.533, 0.533
) s
JOIN ingredients i
  ON i.id = s.ingredient_id
 AND i.name = s.ingredient_name
ON DUPLICATE KEY UPDATE
  unit_label_ko = VALUES(unit_label_ko),
  grams_per_unit = VALUES(grams_per_unit),
  edible_grams_per_unit = VALUES(edible_grams_per_unit),
  is_default = ingredient_units.is_default;

COMMIT;

-- 적용 후 확인:
-- SELECT COUNT(*) AS seeded_count
-- FROM ingredient_units iu
-- JOIN (
--   SELECT 167 AS ingredient_id, '공기' AS normalized_unit_label
--   UNION ALL SELECT 98, '모'
--   UNION ALL SELECT 321, '개'
--   UNION ALL SELECT 110, '개'
--   UNION ALL SELECT 420, '큰술'
--   UNION ALL SELECT 61, '개'
--   UNION ALL SELECT 278, '모'
--   UNION ALL SELECT 324, '개'
--   UNION ALL SELECT 588, 'ml'
--   UNION ALL SELECT 380, 'ml'
--   UNION ALL SELECT 353, 'ml'
--   UNION ALL SELECT 353, '컵'
--   UNION ALL SELECT 585, 'g'
--   UNION ALL SELECT 241, '개'
--   UNION ALL SELECT 88, 'kg'
--   UNION ALL SELECT 157, 'ml'
--   UNION ALL SELECT 192, '알'
--   UNION ALL SELECT 477, 'kg'
--   UNION ALL SELECT 601, '장'
--   UNION ALL SELECT 78, '개'
--   UNION ALL SELECT 353, 'l'
--   UNION ALL SELECT 314, 'ml'
-- ) s
--   ON iu.ingredient_id = s.ingredient_id
--  AND iu.normalized_unit_label = s.normalized_unit_label;

-- 복구/수정:
--   - 권장: 값이 틀린 단위는 DELETE보다 같은 key로 재실행하여 grams_per_unit 값을 보정한다.
--   - 정말 되돌려야 하면 아래 DELETE를 검수 후 실행한다. 기존 default 단위 삭제를 막기 위해 is_default=0 조건을 둔다.
--
-- DELETE iu
-- FROM ingredient_units iu
-- JOIN (
--   SELECT 167 AS ingredient_id, '공기' AS normalized_unit_label
--   UNION ALL SELECT 98, '모'
--   UNION ALL SELECT 321, '개'
--   UNION ALL SELECT 110, '개'
--   UNION ALL SELECT 420, '큰술'
--   UNION ALL SELECT 61, '개'
--   UNION ALL SELECT 278, '모'
--   UNION ALL SELECT 324, '개'
--   UNION ALL SELECT 588, 'ml'
--   UNION ALL SELECT 380, 'ml'
--   UNION ALL SELECT 353, 'ml'
--   UNION ALL SELECT 353, '컵'
--   UNION ALL SELECT 585, 'g'
--   UNION ALL SELECT 241, '개'
--   UNION ALL SELECT 88, 'kg'
--   UNION ALL SELECT 157, 'ml'
--   UNION ALL SELECT 192, '알'
--   UNION ALL SELECT 477, 'kg'
--   UNION ALL SELECT 601, '장'
--   UNION ALL SELECT 78, '개'
--   UNION ALL SELECT 353, 'l'
--   UNION ALL SELECT 314, 'ml'
-- ) s
--   ON iu.ingredient_id = s.ingredient_id
--  AND iu.normalized_unit_label = s.normalized_unit_label
-- WHERE iu.is_default = 0;
