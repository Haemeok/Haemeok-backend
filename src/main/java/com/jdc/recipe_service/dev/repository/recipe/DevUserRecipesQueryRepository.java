package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.lang.Nullable;

import java.util.List;

/**
 * Dev V3 user/me recipes query repository.
 *
 * 운영 {@code RecipeRepository.findByUserIdAndIsPrivateFalse} / {@code findCompletedRecipesByUserId} 등을
 * dev 정책({@link DevRecipeQueryPredicates#accessibleBy})으로 통합:
 *  - viewer == owner → 자신의 ACTIVE PRIVATE/RESTRICTED 모두 노출
 *  - viewer != owner (or anonymous) → ACTIVE+PUBLIC+LISTED만 노출 (RESTRICTED 누수 차단)
 *  - non-ACTIVE는 owner도 차단 (admin 우회 방지)
 *
 * 운영의 owner-all-visible 정책과 다름 — dev V3는 ACTIVE에 한정. invariant 일관성 우선.
 *
 * imageReady (READY 또는 null + AI는 imageKey 필수) 조건은 운영 패턴 그대로.
 */
public interface DevUserRecipesQueryRepository {

    /**
     * targetUserId 소유의 레시피를 viewer 시점에서 접근 가능한 것만 페이지로 반환.
     *
     * @param sourceTypes null/empty면 source filter 없음, 있으면 source IN (...)
     */
    Page<Recipe> findUserRecipesAccessible(Long targetUserId,
                                            @Nullable Long viewerId,
                                            @Nullable List<RecipeSourceType> sourceTypes,
                                            Pageable pageable);
}
