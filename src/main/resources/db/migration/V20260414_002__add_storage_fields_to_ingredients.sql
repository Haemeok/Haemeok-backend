-- 기능 ④-a: 재료 보관정보 필드 (자유 텍스트)

ALTER TABLE ingredients
    ADD COLUMN storage_method TEXT NULL;
