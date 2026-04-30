package com.jdc.recipe_service.dev.service.interaction;

import com.jdc.recipe_service.dev.policy.recipe.DevRecipeAccessPolicy;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import lombok.RequiredArgsConstructor;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * Dev V3 상호작용(like/favorite/rating/comment) 권한 게이트.
 *
 * 운영 service들은 visibility 검증 없이 {@code recipeRepository.findById}만으로 진행 — RESTRICTED/PRIVATE/non-ACTIVE
 * 레시피에 누구나 like/favorite/rating/comment 가능 (운영의 잠재 누수). dev V3는 이 게이트로 차단.
 *
 * 정책: {@link DevRecipeAccessPolicy#isAccessibleBy} 와 동일 ({@code accessibleBy(viewerId)}):
 *  - viewer == owner → ACTIVE PRIVATE/RESTRICTED 모두 상호작용 가능
 *  - viewer != owner (or anonymous) → ACTIVE+PUBLIC+LISTED만
 *  - non-ACTIVE는 owner도 차단 (admin 우회 방지)
 *
 * 에러 매핑:
 *  - 레시피 없음 → RECIPE_NOT_FOUND (404)
 *  - non-ACTIVE → RECIPE_NOT_FOUND (404, "존재하지 않는 것처럼" — V2 detail 가드 패턴 정합)
 *  - PRIVATE/RESTRICTED non-owner → RECIPE_PRIVATE_ACCESS_DENIED (403, 명시적 차단)
 */
@Component
@RequiredArgsConstructor
public class DevRecipeAccessValidator {

    private final RecipeRepository recipeRepository;

    /**
     * Recipe 조회 + 상호작용 가능 여부 검증. 통과 시 entity 반환 (호출자가 재사용 가능).
     *
     * @param recipeId 대상 레시피
     * @param viewerId 상호작용 시도자 (anonymous면 null — 사실상 모든 상호작용은 인증 필수라 null 거의 없음)
     * @throws CustomException 위 에러 매핑대로
     */
    @Transactional(readOnly = true)
    public Recipe loadAndCheckInteractable(Long recipeId, @Nullable Long viewerId) {
        Recipe recipe = recipeRepository.findById(recipeId)
                .orElseThrow(() -> new CustomException(ErrorCode.RECIPE_NOT_FOUND));

        if (DevRecipeAccessPolicy.isAccessibleBy(recipe, viewerId)) {
            return recipe;
        }

        // 차단 — 에러 분기:
        // non-ACTIVE는 RECIPE_NOT_FOUND (admin 조치된 거 사용자에게 노출 안 함)
        if (recipe.getLifecycleStatus() != RecipeLifecycleStatus.ACTIVE) {
            throw new CustomException(ErrorCode.RECIPE_NOT_FOUND);
        }
        // ACTIVE인데 차단 = PRIVATE/RESTRICTED non-owner
        throw new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
    }
}
