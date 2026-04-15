-- 기능 ⑤: YouTube 레시피 clone/remix
-- ADR: docs/decisions/0003-youtube-recipe-clone-approach.md

-- 1) 원본 레시피 참조 컬럼
ALTER TABLE recipes
    ADD COLUMN origin_recipe_id BIGINT NULL;

-- 2) 원본이 삭제되면 복제본의 origin은 NULL로 (복제본 자체는 살아남음)
ALTER TABLE recipes
    ADD CONSTRAINT fk_recipes_origin
    FOREIGN KEY (origin_recipe_id) REFERENCES recipes(id)
    ON DELETE SET NULL;

-- 3) "한 원본 × 한 사람 = 복제 1번"을 DB 레벨에서 강제
--    MySQL의 unique는 NULL을 서로 다른 값으로 취급하므로,
--    복제본이 아닌 일반 레시피(origin_recipe_id=NULL)는 영향 없음.
--    이 제약이 만드는 복합 인덱스가 `WHERE origin_recipe_id = ?` 쿼리에도 leftmost prefix로 동작.
ALTER TABLE recipes
    ADD CONSTRAINT uq_recipes_origin_user
    UNIQUE (origin_recipe_id, user_id);
