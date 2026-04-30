-- 기능: dev V3 추출/생성 작업의 quota 차감 날짜 저장.
-- 배경: async 작업이 시작일 늦은 시각(예: 23:55)에 차감되고 자정을 넘긴 시각(00:05)에 실패할 때,
--       refund가 LocalDate.now()로 오늘 quota counter를 환불하면 시작일 손실 + 다음날 부풀림이 발생.
-- 해결: 차감 시점 날짜를 job에 저장하고, 환불은 그 날짜 기준으로 수행.
--
-- nullable: dev V3에서만 채움. 운영 V1/V2 (RecipeService.togglePrivacy 등)는 기존 1일 단위 환불 로직 유지.

ALTER TABLE recipe_generation_jobs
    ADD COLUMN quota_used_on DATE NULL COMMENT 'quota 차감 날짜 (cross-midnight 환불 정확성 보장용, dev V3 전용)';
