package com.jdc.recipe_service.dev.repository.recipe;

import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;

import java.util.List;

/**
 * Dev V3 ingredient detail의 top recipe 조회용 repository.
 *
 * 운영 {@link com.jdc.recipe_service.domain.repository.RecipeQueryRepositoryImpl#findTopByIngredientId}의
 * dev 버전. 운영의 {@code recipe.isPrivate.isFalse()} 자리에 4-enum 정책(PUBLIC+LISTED+ACTIVE) 적용.
 *
 * 응답 DTO는 운영 {@link RecipeSimpleDto} 그대로 재사용 — ingredient detail의 top recipe에서 4 enum
 * 노출 가치가 낮음(어차피 모두 PUBLIC+LISTED+ACTIVE 동일 값). dev 핵심 가치인 RESTRICTED 누수 차단은
 * query 레벨에서 보장됨.
 */
public interface DevTopRecipeQueryRepository {

    /**
     * 특정 ingredient를 사용하는 dev 정책 통과 레시피의 인기순 top N.
     *  - lifecycle=ACTIVE && visibility=PUBLIC && listing=LISTED
     *  - imageStatus=READY 또는 null
     *  - popularityScore DESC, id DESC
     */
    List<RecipeSimpleDto> findTopByIngredientIdDev(Long ingredientId, int limit);
}
