package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.IngredientUnit;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Collection;
import java.util.List;

/**
 * IngredientUnit JPA repo.
 *
 * <p>1.2 read-only path 진입점. dev calculator의 path 2(C' bypass row의 unit→ingredient
 * canonical 복원)에서 batch lookup용으로 사용. {@link IngredientUnit#getIngredient()} 관계가
 * LAZY라 그냥 로드하면 N+1 또는 트랜잭션 밖 lazy 예외가 나므로, 진입 메서드에서
 * {@code @EntityGraph}로 ingredient를 같이 가져온다.
 */
@Repository
public interface IngredientUnitRepository extends JpaRepository<IngredientUnit, Long> {

    @EntityGraph(attributePaths = {"ingredient"})
    List<IngredientUnit> findAllByIdIn(Collection<Long> ids);

    /**
     * 특정 ingredient 집합에 속한 unit만 fetch (dev write lookup용).
     * findAll() 대신 이 메서드로 좁히면 ingredient_units 전체 ~3945 row 로딩을 피한다.
     */
    @EntityGraph(attributePaths = {"ingredient"})
    List<IngredientUnit> findAllByIngredientIdIn(Collection<Long> ingredientIds);
}
