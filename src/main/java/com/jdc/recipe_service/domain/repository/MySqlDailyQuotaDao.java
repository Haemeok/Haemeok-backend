package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.config.QuotaProperties;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.annotation.Profile;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;

@Repository
@Profile("prod")
@RequiredArgsConstructor
@Slf4j
public class MySqlDailyQuotaDao implements DailyQuotaDao {

    private final JdbcTemplate jdbc;
    private final QuotaProperties props;

    @Override
    public boolean tryConsume(Long userId) {
        var today = LocalDate.now(props.zoneId());
        int limit = props.getPerDay();

        log.info("Try Consume - User: {}, Today: {}, Limit: {}", userId, today, limit);

        int updated = jdbc.update(
                """
                UPDATE user_daily_ai_usage_counter 
                SET used_count = used_count + 1 
                WHERE user_id = ? AND used_on = ? AND used_count < ?
                """,
                userId, today, limit
        );

        if (updated > 0) {
            log.info("-> Update 성공 (카운트 증가)");
            return true;
        }

        try {
            jdbc.update(
                    """
                    INSERT INTO user_daily_ai_usage_counter (user_id, used_on, used_count)
                    VALUES (?, ?, 1)
                    """,
                    userId, today
            );
            log.info("-> Insert 성공 (첫 사용)");
            return true;
        } catch (DuplicateKeyException e) {
            log.warn("-> 차단됨: 한도 초과 (Limit: {}, User: {})", limit, userId);
            return false;
        }
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