package com.jdc.recipe_service.domain.repository.chat;

import com.jdc.recipe_service.domain.entity.chat.ChatDailyUsage;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDate;
import java.util.Optional;

public interface ChatDailyUsageRepository extends JpaRepository<ChatDailyUsage, Long> {

    Optional<ChatDailyUsage> findByUserIdAndUsageDate(Long userId, LocalDate usageDate);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query(value = """
            UPDATE chat_daily_usage
            SET call_count = call_count + 1,
                updated_at = CURRENT_TIMESTAMP(6)
            WHERE user_id = :userId
              AND usage_date = :usageDate
              AND call_count < :limit
            """,
            nativeQuery = true)
    int incrementUsageIfBelowLimit(@Param("userId") Long userId,
                                   @Param("usageDate") LocalDate usageDate,
                                   @Param("limit") int limit);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query(value = "INSERT IGNORE INTO chat_daily_usage (user_id, usage_date, call_count) " +
            "VALUES (:userId, :usageDate, 0)",
            nativeQuery = true)
    void ensureDailyUsageRow(@Param("userId") Long userId,
                             @Param("usageDate") LocalDate usageDate);
}
