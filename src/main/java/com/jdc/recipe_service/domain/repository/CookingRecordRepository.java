package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.CookingRecord;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Repository
public interface CookingRecordRepository extends JpaRepository<CookingRecord, Long> {

    @Query("""
      SELECT cast(c.createdAt as date), sum(c.savings)
      FROM CookingRecord c
      WHERE c.user.id = :userId
        AND year(c.createdAt)  = :year
        AND month(c.createdAt) = :month
      GROUP BY cast(c.createdAt as date)
      """)
    List<Object[]> findMonthlySummaryRaw(
            @Param("userId") Long userId,
            @Param("year")   int year,
            @Param("month")  int month
    );

    /** 연속 요리 일수와 요리 여부 SQL로 계산 */
    // language=MySQL
    @Query(value = """
WITH DistinctCookDates AS (
    SELECT DISTINCT DATE(created_at) AS cook_date
    FROM cooking_records
    WHERE user_id = :userId
      AND DATE(created_at) <= CURRENT_DATE
),
CookedTodayInfo AS (
    SELECT EXISTS(SELECT 1 FROM DistinctCookDates WHERE cook_date = CURRENT_DATE) AS did_cook_today
),
TargetStreakEndDate AS (
    SELECT
        CASE
            WHEN cti.did_cook_today THEN CURRENT_DATE
            ELSE DATE_SUB(CURRENT_DATE, INTERVAL 1 DAY)
        END AS streak_end_date,
        cti.did_cook_today
    FROM CookedTodayInfo cti
),
DateWithGroupKey AS (
    SELECT
        cook_date,
        DATE_SUB(cook_date, INTERVAL ROW_NUMBER() OVER (ORDER BY cook_date ASC) DAY) AS group_key
    FROM DistinctCookDates
),
RelevantStreakGroup AS (
    SELECT
        dgk.group_key
    FROM DateWithGroupKey dgk
    JOIN TargetStreakEndDate tsed ON dgk.cook_date = tsed.streak_end_date
)
SELECT
    COALESCE(
        (SELECT COUNT(dgk.cook_date)
         FROM DateWithGroupKey dgk
         JOIN RelevantStreakGroup rsg ON dgk.group_key = rsg.group_key
         WHERE dgk.cook_date <= (SELECT streak_end_date FROM TargetStreakEndDate)
        ), 0) AS streak,
    (SELECT did_cook_today FROM CookedTodayInfo) AS cooked_today
FROM DUAL
""", nativeQuery = true)
    List<Object[]> findStreakAndTodayFlag(@Param("userId") Long userId, @Param("date") LocalDate date);

    List<CookingRecord> findByUserIdAndCreatedAtBetweenOrderByCreatedAtDesc(
            Long userId, LocalDateTime start, LocalDateTime end
    );

    @Modifying
    @Query("DELETE FROM CookingRecord c WHERE c.recipe.id = :recipeId")
    void deleteByRecipeId(@Param("recipeId") Long recipeId);
}