-- 기능: 재료 보관/페어링/조리법 상세 필드 구조화
-- 상세 노출용 7개 컬럼을 추가한다. 레거시 storage_method는 같은 배포의 V20260419_003 에서 제거.

ALTER TABLE ingredients
    ADD COLUMN storage_location    VARCHAR(50)  NULL COMMENT '보관 위치 (실온/냉장/냉동)',
    ADD COLUMN storage_temperature VARCHAR(255) NULL COMMENT '보관 온도 상세',
    ADD COLUMN storage_duration    VARCHAR(255) NULL COMMENT '보관 기간',
    ADD COLUMN storage_notes       TEXT         NULL COMMENT '보관 참고사항 (자유 텍스트)',
    ADD COLUMN good_pairs          TEXT         NULL COMMENT '잘 어울리는 재료 (슬래시 구분)',
    ADD COLUMN bad_pairs           TEXT         NULL COMMENT '상극 재료 (슬래시 구분)',
    ADD COLUMN recommended_cooking_methods TEXT NULL COMMENT '추천 조리법 (슬래시 구분)';
