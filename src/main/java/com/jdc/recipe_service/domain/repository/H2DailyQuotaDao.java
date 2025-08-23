package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.config.QuotaProperties;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Profile;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;

@Repository
@Profile("!prod")
@RequiredArgsConstructor
public class H2DailyQuotaDao implements DailyQuotaDao {

    private final JdbcTemplate jdbc;
    private final QuotaProperties props;

    @Override
    public boolean tryConsume(Long userId) {
        var today = LocalDate.now(props.zoneId());
        int limit = props.getPerDay();

        String sql = """
            MERGE INTO user_daily_ai_usage_counter t
            USING (SELECT CAST(? AS BIGINT) AS user_id, CAST(? AS DATE) AS used_on) s
            ON (t.user_id = s.user_id AND t.used_on = s.used_on)
            WHEN MATCHED AND t.used_count < ? THEN
                UPDATE SET t.used_count = t.used_count + 1
            WHEN NOT MATCHED THEN
                INSERT (user_id, used_on, used_count) VALUES (?, ?, 1)
            """;

        int updated = jdbc.update(sql,
                userId, today, limit,
                userId, today
        );

        return updated > 0;
    }

    @Override
    public void refundOnce(Long userId) {
        var today = LocalDate.now(props.zoneId());
        jdbc.update(
                "UPDATE user_daily_ai_usage_counter SET used_count = (CASE WHEN used_count > 0 THEN used_count - 1 ELSE 0 END) WHERE user_id=? AND used_on=?",
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