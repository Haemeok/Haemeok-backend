-- 레시피 챗봇 - 운영 설정·대화 기록·일일 쿼터 테이블
-- 참고: docs/chatbot/db_and_logging.md §1. 원문은 Postgres 문법이라 MySQL 8로 변환.
-- 정책: 챗봇 쿼터/킬스위치/분류기 활성/질문 길이는 chat_config에서 DB 단일 관리.
--       기존 DailyQuotaService(레시피 토큰 경제)와 분리된 ChatQuotaService가 chat_daily_usage를 사용한다.

-- =========================================================================
-- 1. chat_config - 운영 중 실시간 변경 가능한 챗봇 설정
-- =========================================================================
CREATE TABLE chat_config (
    id           BIGINT       NOT NULL AUTO_INCREMENT,
    config_key   VARCHAR(100) NOT NULL,
    config_value VARCHAR(500) NOT NULL,
    description  TEXT         NULL,
    updated_at   DATETIME(6)  NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    updated_by   VARCHAR(100) NULL,
    PRIMARY KEY (id),
    UNIQUE KEY uk_chat_config_key (config_key)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;

-- 초기 기본값. ON DUPLICATE KEY UPDATE no-op으로 idempotent 보장 (운영 중 변경 값 보존).
INSERT INTO chat_config (config_key, config_value, description) VALUES
    ('daily_quota_per_user',    '20',   '유저당 일일 챗봇 호출 쿼터'),
    ('chat_enabled',            'true', '챗봇 기능 활성화 여부 (긴급 차단용 킬스위치)'),
    ('mini_classifier_enabled', 'true', 'Mini 분류기 사용 여부 (false면 바로 Pro 호출)'),
    ('max_question_length',     '500',  '유저 질문 최대 길이 (sanitize 후 문자 수)')
ON DUPLICATE KEY UPDATE config_key = config_key;


-- =========================================================================
-- 2. chat_log - 대화 기록 (모니터링/비용 추적/분석/증거)
-- =========================================================================
-- user_id/recipe_id FK 미설정 — user/recipe 삭제 후에도 감사 추적용으로 보존.
CREATE TABLE chat_log (
    id                  BIGINT        NOT NULL AUTO_INCREMENT,

    -- 누가, 어디서
    user_id             BIGINT        NOT NULL,
    recipe_id           BIGINT        NOT NULL,
    session_id          VARCHAR(50)   NULL,

    -- 무엇을 물었고 어떻게 답했나
    question            TEXT          NOT NULL,
    intent              VARCHAR(20)   NOT NULL,
    answer              TEXT          NOT NULL,
    pro_called          TINYINT(1)    NOT NULL,

    -- 성능·비용 추적
    mini_latency_ms     INT           NULL,
    pro_latency_ms      INT           NULL,
    total_latency_ms    INT           NOT NULL,
    mini_input_tokens   INT           NULL,
    mini_output_tokens  INT           NULL,
    pro_input_tokens    INT           NULL,
    pro_cached_tokens   INT           NULL,
    pro_output_tokens   INT           NULL,
    estimated_cost_krw  DECIMAL(10,4) NULL,

    -- 후처리 플래그
    repetition_detected TINYINT(1)    NOT NULL DEFAULT 0,
    answer_truncated    TINYINT(1)    NOT NULL DEFAULT 0,

    -- 보안 플래그
    suspicious          TINYINT(1)    NOT NULL DEFAULT 0,
    suspicious_reason   VARCHAR(100)  NULL,

    -- 프롬프트 버전 (A/B·롤백용)
    classifier_version  VARCHAR(20)   NOT NULL DEFAULT 'v2',
    chat_version        VARCHAR(20)   NOT NULL DEFAULT 'v6',

    -- 에러
    error_message       TEXT          NULL,

    -- 유저 피드백 (후속)
    user_feedback       VARCHAR(20)   NULL,
    feedback_comment    TEXT          NULL,

    -- 시간
    created_at          DATETIME(6)   NOT NULL DEFAULT CURRENT_TIMESTAMP(6),

    PRIMARY KEY (id),
    KEY idx_chat_log_user_created  (user_id, created_at DESC),
    KEY idx_chat_log_recipe_id     (recipe_id),
    KEY idx_chat_log_intent        (intent),
    KEY idx_chat_log_created       (created_at DESC)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;


-- =========================================================================
-- 3. chat_daily_usage - 유저별 일일 호출 카운터
-- =========================================================================
CREATE TABLE chat_daily_usage (
    id         BIGINT      NOT NULL AUTO_INCREMENT,
    user_id    BIGINT      NOT NULL,
    usage_date DATE        NOT NULL,
    call_count INT         NOT NULL DEFAULT 0,
    updated_at DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) ON UPDATE CURRENT_TIMESTAMP(6),
    PRIMARY KEY (id),
    UNIQUE KEY uk_chat_daily_usage_user_date (user_id, usage_date),
    KEY idx_chat_daily_usage_date (usage_date)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_unicode_ci;
