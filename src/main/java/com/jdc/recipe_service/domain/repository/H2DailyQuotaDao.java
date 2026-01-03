package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.type.QuotaType; // ✅ 추가
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
    public boolean tryConsume(Long userId, QuotaType type) {
        var today = LocalDate.now(props.zoneId());
        int limit = props.getPerDay();
        String typeStr = type.name();

        String sql = """
            MERGE INTO user_daily_ai_usage_counter t
            USING (SELECT CAST(? AS BIGINT) AS user_id, CAST(? AS DATE) AS used_on, CAST(? AS VARCHAR) AS quota_type) s
            ON (t.user_id = s.user_id AND t.used_on = s.used_on AND t.quota_type = s.quota_type)
            WHEN MATCHED AND t.used_count < ? THEN
                UPDATE SET t.used_count = t.used_count + 1
            WHEN NOT MATCHED THEN
                INSERT (user_id, used_on, quota_type, used_count) VALUES (?, ?, ?, 1)
            """;

        int updated = jdbc.update(sql,
                userId, today, typeStr,
                limit,
                userId, today, typeStr
        );

        return updated > 0;
    }

    @Override
    public void refundOnce(Long userId, QuotaType type) {
        var today = LocalDate.now(props.zoneId());
        String typeStr = type.name();

        jdbc.update(
                "UPDATE user_daily_ai_usage_counter SET used_count = (CASE WHEN used_count > 0 THEN used_count - 1 ELSE 0 END) WHERE user_id=? AND used_on=? AND quota_type=?",
                userId, today, typeStr
        );
    }

    @Override
    public int remainingToday(Long userId, QuotaType type) {
        var today = LocalDate.now(props.zoneId());
        String typeStr = type.name();

        Integer used = jdbc.query(
                "SELECT used_count FROM user_daily_ai_usage_counter WHERE user_id=? AND used_on=? AND quota_type=?",
                ps -> {
                    ps.setLong(1, userId);
                    ps.setObject(2, today);
                    ps.setString(3, typeStr);
                },
                rs -> rs.next() ? rs.getInt(1) : 0
        );
        return Math.max(0, props.getPerDay() - used);
    }
}