package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.IngredientCandidate;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

/**
 * ingredient_candidates JPA repo.
 *
 * <p>1단계는 row 단위 단순 INSERT. dedupe/upsert는 후속 batch가 raw_name + raw_unit_text + ingredient_id
 * 기준으로 묶어 처리한다. 여기서는 dev persist가 row를 만들고 RecipeIngredient.ingredient_candidate_id
 * FK로 연결하는 진입점만 제공.
 *
 * <p><b>candidate_type 값</b>: "UNIT" (DB 재료 hit + unit miss) | "INGREDIENT" (DB 재료 미스).
 * <p><b>status 초기값</b>: "PENDING". 후속 resolution batch가 RESOLVED/REJECTED로 갱신.
 */
@Repository
public interface IngredientCandidateRepository extends JpaRepository<IngredientCandidate, Long> {

    @Modifying(flushAutomatically = true)
    @Query("UPDATE IngredientCandidate c SET c.sourceRecipeId = null WHERE c.sourceRecipeId = :recipeId")
    int clearSourceRecipeId(@Param("recipeId") Long recipeId);
}
