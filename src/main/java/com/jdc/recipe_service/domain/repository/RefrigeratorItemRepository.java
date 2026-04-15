package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RefrigeratorItem;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface RefrigeratorItemRepository extends JpaRepository<RefrigeratorItem, Long> {

    /** 페이징: 전체 */
    Page<RefrigeratorItem> findByUserId(Long userId, Pageable pageable);

    /** 페이징 + 카테고리 필터 */
    Page<RefrigeratorItem> findByUserIdAndIngredientCategoryIgnoreCase(
            Long userId, String category, Pageable pageable);

    /** 특정 재료가 이미 있는지 체크 */
    Optional<RefrigeratorItem> findByUserIdAndIngredientId(Long userId, Long ingredientId);

    /** 삭제용 */
    void deleteByUserIdAndIngredientId(Long userId, Long ingredientId);

    /** 전체 리스트 조회*/
    List<RefrigeratorItem> findAllByUserId(Long userId);

    /**
     * 특정 레시피에 사용된 재료 중, 사용자의 냉장고에 보유 중인 재료 ID 목록.
     * RecipeIngredient.ingredient가 null인(커스텀 재료) 항목은 자연스럽게 제외된다.
     */
    @Query("SELECT rf.ingredient.id FROM RefrigeratorItem rf " +
            "WHERE rf.user.id = :userId " +
            "AND rf.ingredient.id IN (" +
            "  SELECT ri.ingredient.id FROM RecipeIngredient ri " +
            "  WHERE ri.recipe.id = :recipeId AND ri.ingredient IS NOT NULL" +
            ")")
    List<Long> findIngredientIdsInFridgeForRecipe(@Param("userId") Long userId,
                                                  @Param("recipeId") Long recipeId);
}
