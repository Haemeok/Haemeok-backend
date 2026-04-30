-- 기능: YouTube 추출 시 판단 메타데이터(자막/설명/댓글 신호, Gemini fallback 여부, 신뢰도, 차감 토큰) 저장 테이블 신설.
-- 원문(자막 텍스트, 댓글 본문)은 저장하지 않는다. 응답 노출용 evidence_level과 환불 단위 token_cost만 보관.
-- recipe와 1:1 매핑 (uq_youtube_extraction_recipe_id), recipe 삭제 시 ON DELETE CASCADE.
-- created_at/updated_at은 BaseTimeEntity와 정합을 위해 NOT NULL.

CREATE TABLE recipe_youtube_extraction_info (
    id                          BIGINT      NOT NULL AUTO_INCREMENT,
    recipe_id                   BIGINT      NOT NULL,
    has_subtitle                BOOLEAN     NOT NULL DEFAULT FALSE,
    has_description_ingredient  BOOLEAN     NOT NULL DEFAULT FALSE,
    has_comment_ingredient      BOOLEAN     NOT NULL DEFAULT FALSE,
    used_gemini_analysis        BOOLEAN     NOT NULL DEFAULT FALSE,
    evidence_level              VARCHAR(20) NOT NULL COMMENT 'HIGH | MEDIUM | LOW',
    token_cost                  INT         NOT NULL COMMENT '실제 차감된 토큰 수 (2 또는 5)',
    created_at                  DATETIME(6) NOT NULL,
    updated_at                  DATETIME(6) NOT NULL,
    PRIMARY KEY (id),
    CONSTRAINT uq_youtube_extraction_recipe_id UNIQUE (recipe_id),
    CONSTRAINT fk_youtube_extraction_recipe
        FOREIGN KEY (recipe_id) REFERENCES recipes (id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
