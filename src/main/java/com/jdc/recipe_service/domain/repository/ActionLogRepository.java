package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.ActionLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;

public interface ActionLogRepository extends JpaRepository<ActionLog, Long> {
    @Query("SELECT COUNT(DISTINCT a.guestUuid) FROM ActionLog a WHERE a.createdAt >= :startOfDay")
    long countTodayUniqueVisitors(@Param("startOfDay") LocalDateTime startOfDay);

    // 2. 오늘 총 클릭수
    @Query("SELECT COUNT(a) FROM ActionLog a WHERE a.createdAt >= :startOfDay")
    long countTodayTotalClicks(@Param("startOfDay") LocalDateTime startOfDay);

    // 3. [추가] 전체 누적 순 방문자
    @Query("SELECT COUNT(DISTINCT a.guestUuid) FROM ActionLog a")
    long countTotalUniqueVisitors();

    // 4. [추가] 전체 누적 클릭수
    @Query("SELECT COUNT(a) FROM ActionLog a")
    long countTotalClicks();
}