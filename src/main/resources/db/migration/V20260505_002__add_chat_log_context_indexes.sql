-- Additive index change for chatbot history lookups.
-- Rollout: safe to deploy before or after the application code because it does not change row shape.
-- Verify with EXPLAIN on findRecentForContext/findForDisplay query patterns.
-- Recovery: roll forward by leaving indexes in place; drop only in a later contract migration if write overhead is confirmed.

ALTER TABLE chat_log
    ADD INDEX idx_chat_log_context (user_id, recipe_id, session_id, created_at DESC),
    ADD INDEX idx_chat_log_display (user_id, recipe_id, created_at DESC);
