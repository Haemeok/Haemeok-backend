-- 기능: 구조화된 보관 필드 7개(storage_location/temperature/duration/notes + good_pairs/bad_pairs/recommended_cooking_methods)로
-- 전환 완료됨에 따라 레거시 단일 자유 텍스트 컬럼 storage_method 제거.

ALTER TABLE ingredients
    DROP COLUMN storage_method;
