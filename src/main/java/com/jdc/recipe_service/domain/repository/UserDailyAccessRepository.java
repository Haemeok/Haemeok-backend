package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.UserDailyAccess;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.LocalDateTime;

public interface UserDailyAccessRepository extends JpaRepository<UserDailyAccess, Long> {

    @Modifying
    @Transactional
    @Query(value = "INSERT IGNORE INTO user_daily_access (user_id, access_date, os_type, created_at) " +
            "VALUES (:userId, :accessDate, :osType, :createdAt)", nativeQuery = true)
    void insertIgnoreVisit(@Param("userId") Long userId,
                           @Param("accessDate") LocalDate accessDate,
                           @Param("osType") String osType,
                           @Param("createdAt") LocalDateTime createdAt);
}