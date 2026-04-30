-- 기능: 비동기 생성 작업의 토큰 비용/Gemini fallback 여부/요청 이미지 모델을 job 단위로 추적.
-- 실패 시 정확한 환불 단위(2 또는 5)와 어떤 모델 요청이었는지 진단에 사용.
-- 모두 nullable: 신규 dev API에서만 채우고, 기존 prod 경로는 NULL 유지.

ALTER TABLE recipe_generation_jobs
    ADD COLUMN token_cost              INT          NULL COMMENT '이 job이 차감한 토큰 수 (실패 시 환불 단위)',
    ADD COLUMN used_gemini_analysis    BOOLEAN      NULL COMMENT 'YouTube 추출 시 Gemini 분석 fallback 사용 여부',
    ADD COLUMN image_generation_model  VARCHAR(100) NULL COMMENT '요청한 이미지 생성 모델 식별자';
