package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
public interface RecipeIngredientRepository extends JpaRepository<RecipeIngredient, Long> {
    /**
     * 레시피 한 건의 모든 RecipeIngredient를 ID 오름차순(=삽입 순서)으로 결정적으로 반환.
     *
     * <p>여러 호출자가 같은 recipeId에 대해 이 메서드를 두 번 부를 수 있는데(V2 detail + dev detail
     * 같은 read path), 같은 트랜잭션에서도 query result는 캐시되지 않으므로 명시 ORDER BY가 없으면
     * 두 호출이 다른 순서를 받을 수 있다. dev raw-first 적용은 V2 base의 ingredient 인덱스에
     * 의존하므로 ORDER BY 보장이 필수.
     *
     * <p>auto-increment id 기준이라 실질적으로 InnoDB가 이미 반환하던 자연 순서와 동일 — V2의
     * 기존 표시 순서가 그대로 유지된다.
     */
    @EntityGraph(attributePaths = {"ingredient"})
    @Query("SELECT ri FROM RecipeIngredient ri WHERE ri.recipe.id = :recipeId ORDER BY ri.id ASC")
    List<RecipeIngredient> findByRecipeId(@Param("recipeId") Long recipeId);

    @EntityGraph(attributePaths = {"ingredient"})
    List<RecipeIngredient> findByRecipeIdIn(List<Long> recipeIds);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM RecipeIngredient ri WHERE ri.recipe.id = :recipeId")
    @Transactional
    void deleteByRecipeId(@Param("recipeId") Long recipeId);

    @Query("SELECT ri.ingredient.id, COUNT(ri) " +
            "FROM RecipeIngredient ri " +
            "WHERE ri.ingredient IS NOT NULL " +
            "GROUP BY ri.ingredient.id")
    List<Object[]> countIngredientsUsage();

    @Query("SELECT ri FROM RecipeIngredient ri " +
            "WHERE ri.ingredient IS NULL " +
            "AND ri.customLink IS NULL")
    List<RecipeIngredient> findCustomIngredientsNeedLink(Pageable pageable);

    @Query("SELECT DISTINCT r.customName FROM RecipeIngredient r " +
            "WHERE r.ingredient IS NULL " +
            "AND r.customLink IS NULL " +
            "AND r.customName IS NOT NULL " +
            "AND r.customName <> ''")
    List<String> findDistinctNamesNeedLink(Pageable pageable);

    @Modifying(clearAutomatically = true)
    @Transactional
    @Query("UPDATE RecipeIngredient ri " +
            "SET ri.customLink = :link " +
            "WHERE ri.customName = :name " +
            "AND ri.customLink IS NULL " +
            "AND ri.ingredient IS NULL")
    int updateLinkByCustomName(@Param("name") String name, @Param("link") String link);
}