package com.jdc.recipe_service.domain.repository;

public interface DailyQuotaDao {
    boolean tryConsume(Long userId);
    void refundOnce(Long userId);
    int remainingToday(Long userId);
}
