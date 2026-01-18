package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.config.QuotaProperties;
import com.jdc.recipe_service.domain.type.QuotaType;
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
    private final QuotaProperties props; // 시간대(zoneId) 확인용으로만 사용

    @Override
    public boolean tryConsume(Long userId, QuotaType type, int limit) { // limit 받음
        var today = LocalDate.now(props.zoneId());
        String typeStr = type.name();

        log.info("Try Consume - User: {}, Type: {}, Today: {}, Limit: {}", userId, typeStr, today, limit);

        // [수정] 쿼리 파라미터에 limit 전달
        int updated = jdbc.update(
                """
                UPDATE user_daily_ai_usage_counter 
                SET used_count = used_count + 1 
                WHERE user_id = ? AND used_on = ? AND quota_type = ? AND used_count < ?
                """,
                userId, today, typeStr, limit
        );

        if (updated > 0) return true;

        try {
            jdbc.update(
                    """
                    INSERT INTO user_daily_ai_usage_counter (user_id, used_on, quota_type, used_count)
                    VALUES (?, ?, ?, 1)
                    """,
                    userId, today, typeStr
            );
            return true;
        } catch (DuplicateKeyException e) {
            return false;
        }
    }

    @Override
    public void refundOnce(Long userId, QuotaType type) {
        // 기존과 동일
        var today = LocalDate.now(props.zoneId());
        String typeStr = type.name();
        jdbc.update(
                "UPDATE user_daily_ai_usage_counter SET used_count = GREATEST(used_count - 1, 0) WHERE user_id=? AND used_on=? AND quota_type=?",
                userId, today, typeStr
        );
    }

    @Override
    public int remainingToday(Long userId, QuotaType type, int limit) { // limit 받음
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

        return Math.max(0, limit - used);
    }
}