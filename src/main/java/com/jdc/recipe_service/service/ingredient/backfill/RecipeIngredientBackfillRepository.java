package com.jdc.recipe_service.service.ingredient.backfill;

import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * 정규화 백필 전용 RecipeIngredient 조회.
 *
 * <p>Repository는 "대상 row 조회"만 담당한다. 정규화 결정 로직은 {@link RecipeIngredientBackfillService}와
 * normalize helper(QuantityParser/UnitNormalizer 등)에 위임 — SQL에 비즈니스 로직을 박지 않는다.
 */
@Repository
public interface RecipeIngredientBackfillRepository extends JpaRepository<RecipeIngredient, Long> {

    /**
     * 정규화 백필 대상 row chunk 조회.
     *
     * <p><b>대상 조건</b>
     * <ul>
     *   <li>{@code id > :lastId} — keyset 페이징, 정렬 안정</li>
     *   <li>{@code ingredient_id IS NOT NULL} — C' bypass / UNRESOLVED / CUSTOM(custom row) 제외</li>
     *   <li>{@code raw_quantity_text IS NOT NULL AND raw_unit_text IS NOT NULL} — 1차 백필이 raw_*를 채웠음</li>
     *   <li>아래 중 하나의 status 조건:
     *     <ul>
     *       <li>{@code resolution_status IS NULL} — 1차 백필 누락</li>
     *       <li>{@code resolution_status = 'MAPPED'} 인데 새 필드(amount/unit_id/grams) 중 하나라도 null</li>
     *       <li>{@code resolution_status = 'PARTIAL'} 인데 새 필드 모두 null — 시도조차 안 됨</li>
     *       <li>알 수 없는 status (대문자 정규화 실패 케이스 방어)</li>
     *     </ul>
     *   </li>
     * </ul>
     *
     * <p><b>제외</b>: UNRESOLVED (시도 후 실패한 최종 상태), CUSTOM (사용자 의도 final).
     *
     * <p>{@code @EntityGraph(ingredient)}로 N+1 차단 — service가 row.getIngredient().getId()를 곧바로 사용.
     */
    @EntityGraph(attributePaths = {"ingredient"})
    @Query("""
            SELECT ri FROM RecipeIngredient ri
            WHERE ri.id > :lastId
              AND ri.ingredient IS NOT NULL
              AND ri.rawQuantityText IS NOT NULL
              AND ri.rawUnitText IS NOT NULL
              AND (
                ri.resolutionStatus IS NULL
                OR (
                  ri.resolutionStatus = 'MAPPED'
                  AND (ri.amountValue IS NULL OR ri.ingredientUnitId IS NULL OR ri.normalizedGrams IS NULL)
                )
                OR (
                  ri.resolutionStatus = 'PARTIAL'
                  AND ri.amountValue IS NULL AND ri.ingredientUnitId IS NULL AND ri.normalizedGrams IS NULL
                )
                OR ri.resolutionStatus NOT IN ('MAPPED', 'PARTIAL', 'UNRESOLVED', 'CUSTOM')
              )
            ORDER BY ri.id ASC
            """)
    List<RecipeIngredient> findNormalizationBackfillTargets(@Param("lastId") long lastId,
                                                             org.springframework.data.domain.Pageable pageable);
}
