-- recipes 자식 테이블 중 ON DELETE 정책이 NO ACTION으로 남아 있던 두 FK를 CASCADE로 통일한다.
-- 기존 RecipeService.deleteRecipe 경로(JPQL bulk delete)가 이 두 테이블에 row가 있을 때
-- FK violation으로 실패하던 문제를 해결한다. 다른 12개 자식 테이블은 이미 ON DELETE CASCADE다.
--
-- App 호환성: parent(recipes) row가 살아 있는 동안 child 동작은 동일.
-- 변경되는 시점은 오직 recipes row 삭제 시점이고, "FK violation으로 거부" → "child 자동 삭제"로
-- strict하게 더 관대해지는 방향이라 구 버전 앱과 새 스키마가 만나도 회귀 위험 없음.
--
-- 운영 주의: MySQL에서 DROP FOREIGN KEY + ADD CONSTRAINT는 metadata lock을 잡는다.
-- 트래픽이 낮은 시간대에 적용하고, 적용 직전에 long-running transaction이 없는지
-- (information_schema.innodb_trx, SHOW PROCESSLIST) 확인하는 것을 권장한다.

ALTER TABLE recipe_youtube_info
    DROP FOREIGN KEY FKh6nuvr74hlnrwtjx7rlk7ppcn,
    ADD CONSTRAINT FK_recipe_youtube_info_recipe
        FOREIGN KEY (recipe_id) REFERENCES recipes (id) ON DELETE CASCADE;

ALTER TABLE recipe_access
    DROP FOREIGN KEY FKrv5rsk2xhoflktwskbipk0a9s,
    ADD CONSTRAINT FK_recipe_access_recipe
        FOREIGN KEY (recipe_id) REFERENCES recipes (id) ON DELETE CASCADE;
