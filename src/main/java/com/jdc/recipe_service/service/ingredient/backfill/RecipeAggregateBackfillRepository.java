package com.jdc.recipe_service.service.ingredient.backfill;

import com.jdc.recipe_service.domain.entity.Recipe;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * Recipe aggregate 백필 전용 조회 repository.
 *
 * <p>대상은 모든 active Recipe — id 기반 keyset 페이징. ingredient는 별도 IN 절 batch fetch로
 * N+1 차단(LAZY 차단을 위해 service가 unit fetch도 batch). soft-deleted 레시피는 SQLRestriction이
 * 자동 적용된다고 가정.
 */
@Repository
public interface RecipeAggregateBackfillRepository extends JpaRepository<Recipe, Long> {

    @Query("""
            SELECT r FROM Recipe r
            WHERE r.id > :lastId
            ORDER BY r.id ASC
            """)
    List<Recipe> findAggregateBackfillTargets(@Param("lastId") long lastId, Pageable pageable);
}
