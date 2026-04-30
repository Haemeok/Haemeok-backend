-- 운영 수동 실행 스크립트 (Flyway 자동 적용 대상 아님)
-- ===========================================================================
-- 기능: 기존 AI/YOUTUBE 출처 레시피 중 이미지가 있는 row의 image_generation_model을
--       'gemini-2.5-flash-image'로 백필.
-- 배경: V20260426_002 마이그레이션이 컬럼만 추가한다. 백필은 hot table(recipes)
--       락 리스크와 Flyway 계정의 CREATE ROUTINE 권한 의존을 피하기 위해
--       이 스크립트를 운영자가 수동으로 실행한다.
-- 실행: mysql -u <admin_user> -p <db_name> < backfill_recipe_image_generation_model.sql
--       또는 운영 DB 클라이언트에서 통째로 실행.
-- 권장 시점: 트래픽 낮은 시간대.
-- ===========================================================================

-- 0) 사전 검증: 백필 대상 row 수 확인
SELECT COUNT(*) AS rows_to_backfill
FROM recipes
WHERE image_generation_model IS NULL
  AND image_key IS NOT NULL
  AND source IN ('AI', 'YOUTUBE');

-- 1) Chunked 백필 (1000건 단위)
--    procedure가 로컬 세션 안에서만 정의되므로 운영자 권한이 있다면 안전.
--    멱등적: image_generation_model IS NULL 조건으로 재실행 시 이미 채워진 row는 건너뛴다.

DROP PROCEDURE IF EXISTS sp_backfill_recipe_image_model;

DELIMITER $$
CREATE PROCEDURE sp_backfill_recipe_image_model()
BEGIN
    DECLARE v_rows INT DEFAULT 1;
    WHILE v_rows > 0 DO
        UPDATE recipes
        SET image_generation_model = 'gemini-2.5-flash-image'
        WHERE image_generation_model IS NULL
          AND image_key IS NOT NULL
          AND source IN ('AI', 'YOUTUBE')
        LIMIT 1000;
        SET v_rows = ROW_COUNT();
    END WHILE;
END$$
DELIMITER ;

CALL sp_backfill_recipe_image_model();

DROP PROCEDURE sp_backfill_recipe_image_model;

-- 2) 사후 검증: 백필 결과 확인
SELECT COUNT(*) AS backfilled_count
FROM recipes
WHERE image_generation_model = 'gemini-2.5-flash-image'
  AND source IN ('AI', 'YOUTUBE');

SELECT COUNT(*) AS still_null_count
FROM recipes
WHERE image_generation_model IS NULL
  AND image_key IS NOT NULL
  AND source IN ('AI', 'YOUTUBE');  -- 0이어야 정상
