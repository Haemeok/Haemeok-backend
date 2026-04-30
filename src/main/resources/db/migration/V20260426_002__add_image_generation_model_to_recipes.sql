-- 기능: 레시피 이미지 생성에 사용된 모델 식별자를 저장.
-- 허용값 (애플리케이션 화이트리스트):
--   gemini-2.5-flash-image
--   gpt-image-2-low
--   gpt-image-2-medium
--   gpt-image-2-high
-- nullable. 사용자 직접 업로드 이미지(USER source)는 AI 모델이 아니므로 NULL 유지.
--
-- 기존 row 백필은 Flyway에서 자동 처리하지 않는다. 운영자가 트래픽 낮은 시간대에
-- src/main/resources/db/manual/backfill_recipe_image_generation_model.sql을 수동 실행한다.
-- (hot table 락 리스크와 Flyway 계정의 CREATE ROUTINE 권한 의존을 피하기 위함.)

ALTER TABLE recipes
    ADD COLUMN image_generation_model VARCHAR(100) NULL COMMENT '이미지 생성 모델 식별자';
