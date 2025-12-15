package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.ActionLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.LocalDateTime;
import java.util.List;

public interface ActionLogRepository extends JpaRepository<ActionLog, Long> {
    @Query("SELECT a.actionType, COUNT(DISTINCT a.guestUuid), COUNT(a) " +
            "FROM ActionLog a " +
            "GROUP BY a.actionType " +
            "ORDER BY COUNT(a) DESC")
    List<Object[]> getStatSummary();

    @Query("SELECT COUNT(DISTINCT a.guestUuid) FROM ActionLog a WHERE a.createdAt >= :startOfDay")
    long countTodayUniqueVisitors(@Param("startOfDay") LocalDateTime startOfDay);

    @Query("SELECT COUNT(a) FROM ActionLog a WHERE a.createdAt >= :startOfDay")
    long countTodayTotalClicks(@Param("startOfDay") LocalDateTime startOfDay);
}