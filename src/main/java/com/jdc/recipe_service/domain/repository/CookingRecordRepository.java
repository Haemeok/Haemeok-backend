package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.CookingRecord;
import org.springframework.data.jpa.repository.*;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
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

    List<CookingRecord> findByUserIdAndCreatedAtBetweenOrderByCreatedAtDesc(
            Long userId, LocalDateTime start, LocalDateTime end
    );

    @Modifying
    @Query("DELETE FROM CookingRecord c WHERE c.ratingId = :ratingId")
    void deleteByRatingId(@Param("ratingId") Long ratingId);

    @Modifying
    @Query("DELETE FROM CookingRecord c WHERE c.recipe.id = :recipeId")
    void deleteByRecipeId(@Param("recipeId") Long recipeId);
}