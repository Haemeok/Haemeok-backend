-- 기존 유저 전원에게 기본 레시피북 생성 + 즐겨찾기 레시피를 기본 폴더로 복사
-- 순수 additive 작업이므로 기존 recipe_favorites 테이블은 건드리지 않음
-- 모든 INSERT/UPDATE는 멱등: NOT EXISTS / WHERE 조건으로 재실행 시에도 안전

-- 1) 모든 기존 유저에게 기본 레시피북 생성
--    uk_recipe_books_one_default_per_user 유니크 제약으로 중복 불가
INSERT INTO recipe_books (user_id, name, is_default, display_order, recipe_count, created_at, updated_at)
SELECT u.id, '저장한 레시피', 1, 0, 0, NOW(6), NOW(6)
FROM users u
WHERE NOT EXISTS (
    SELECT 1 FROM recipe_books rb
    WHERE rb.user_id = u.id AND rb.is_default = 1
);

-- 2) 기존 즐겨찾기를 기본 레시피북으로 복사
--    uk_recipe_book_items_book_recipe 유니크 제약으로 중복 불가
INSERT INTO recipe_book_items (book_id, recipe_id, created_at, updated_at)
SELECT rb.id, rf.recipe_id, rf.created_at, rf.updated_at
FROM recipe_favorites rf
JOIN recipe_books rb ON rb.user_id = rf.user_id AND rb.is_default = 1
WHERE NOT EXISTS (
    SELECT 1 FROM recipe_book_items rbi
    WHERE rbi.book_id = rb.id AND rbi.recipe_id = rf.recipe_id
);

-- 3) 기본 레시피북의 recipe_count 갱신
UPDATE recipe_books rb
SET recipe_count = (
    SELECT COUNT(*)
    FROM recipe_book_items rbi
    WHERE rbi.book_id = rb.id
)
WHERE rb.is_default = 1
  AND rb.recipe_count != (
    SELECT COUNT(*)
    FROM recipe_book_items rbi
    WHERE rbi.book_id = rb.id
);
