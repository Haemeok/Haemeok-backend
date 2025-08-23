package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.config.QuotaProperties;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Profile;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;

@Repository
@Profile("prod")
@RequiredArgsConstructor
public class MySqlDailyQuotaDao implements DailyQuotaDao {

    private final JdbcTemplate jdbc;
    private final QuotaProperties props;

    @Override
    public boolean tryConsume(Long userId) {
        var today = LocalDate.now(props.zoneId());
        int limit = props.getPerDay();
        int updated = jdbc.update(
                """
                INSERT INTO user_daily_ai_usage_counter (user_id, used_on, used_count)
                VALUES (?, ?, 1)
                ON DUPLICATE KEY UPDATE
                  used_count = IF(used_count < ?, used_count + 1, used_count)
                """,
                userId, today, limit
        );
        return updated > 0;
    }

    @Override
    public void refundOnce(Long userId) {
        var today = LocalDate.now(props.zoneId());
        jdbc.update(
                "UPDATE user_daily_ai_usage_counter SET used_count = GREATEST(used_count - 1, 0) WHERE user_id=? AND used_on=?",
                userId, today
        );
    }

    @Override
    public int remainingToday(Long userId) {
        var today = LocalDate.now(props.zoneId());
        Integer used = jdbc.query(
                "SELECT used_count FROM user_daily_ai_usage_counter WHERE user_id=? AND used_on=?",
                ps -> { ps.setLong(1, userId); ps.setObject(2, today); },
                rs -> rs.next() ? rs.getInt(1) : 0
        );
        return Math.max(0, props.getPerDay() - used);
    }
}