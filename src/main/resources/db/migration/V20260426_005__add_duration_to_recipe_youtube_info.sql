-- 기능: YouTube 영상 길이(초) 저장.
-- 정보 부족 시 Gemini 분석 fallback 적합도 판단(예: 70분 초과 영상은 분석 비용 과다로 거부)에 사용.
-- nullable. 기존 row는 NULL 유지.

ALTER TABLE recipe_youtube_info
    ADD COLUMN duration_seconds BIGINT NULL COMMENT '영상 길이 (초)';
