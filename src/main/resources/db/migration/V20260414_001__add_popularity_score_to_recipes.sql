-- 기능 ②: 검색 기본 정렬을 인기순(누적 like_count + favorite_count)으로 변경하기 위한 인덱스 가능 컬럼
-- ADR: docs/decisions/0002-popularity-score-as-regular-column.md

-- 1) 컬럼 추가 (MySQL 8 INSTANT DDL로 즉시 완료, 테이블 리빌드 없음)
ALTER TABLE recipes
    ADD COLUMN popularity_score BIGINT NOT NULL DEFAULT 0;

-- 2) 공개 레시피 인기순 정렬용 복합 인덱스
CREATE INDEX idx_recipes_public_popularity
    ON recipes (lifecycle_status, visibility, listing_status, popularity_score DESC);

-- 3) 기존 row 백필 (2만 row 기준 단일 UPDATE로 수 초 이내)
UPDATE recipes
SET popularity_score = COALESCE(like_count, 0) + COALESCE(favorite_count, 0);
