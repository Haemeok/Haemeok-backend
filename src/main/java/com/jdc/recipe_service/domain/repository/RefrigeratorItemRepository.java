package com.jdc.recipe_service.domain.repository;

import com.jdc.recipe_service.domain.entity.RefrigeratorItem;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
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
}
