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

    // (user_id, usage_date) UNIQUE 기반 원자 증감. 한도 체크는 서비스에서 호출 전에 수행.
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query(value = "INSERT INTO chat_daily_usage (user_id, usage_date, call_count, updated_at) " +
            "VALUES (:userId, :usageDate, 1, CURRENT_TIMESTAMP(6)) " +
            "ON DUPLICATE KEY UPDATE call_count = call_count + 1",
            nativeQuery = true)
    void incrementUsage(@Param("userId") Long userId,
                        @Param("usageDate") LocalDate usageDate);
}
