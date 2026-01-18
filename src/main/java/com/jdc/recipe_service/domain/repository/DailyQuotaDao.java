package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.type.QuotaType;

public interface DailyQuotaDao {
    boolean tryConsume(Long userId, QuotaType type, int limit);
    void refundOnce(Long userId, QuotaType type);
    int remainingToday(Long userId, QuotaType type, int limit);
}