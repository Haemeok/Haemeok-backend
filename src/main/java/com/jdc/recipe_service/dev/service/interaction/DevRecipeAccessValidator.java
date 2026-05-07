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
 * Dev V3 상호작용(like/favorite/rating/comment/조리기록/status) 권한 게이트.
 *
 * <p>운영 service들은 visibility 검증 없이 {@code recipeRepository.findById}만으로 진행 — RESTRICTED/PRIVATE/non-ACTIVE
 * 레시피에 누구나 like/favorite/rating/comment 가능 (운영의 잠재 누수). dev V3는 이 게이트로 차단.
 *
 * <p>V1.x 정책 — {@link DevRecipeAccessPolicy#isViewableBy}: ACTIVE && (owner OR PUBLIC). listingStatus 무시 →
 * <b>PUBLIC+UNLISTED(link-only)도 누구나 상호작용 가능</b> (직접 링크로 접근한 후 댓글/좋아요/저장 등).
 *
 * <ul>
 *   <li>viewer == owner → ACTIVE 자기 글은 visibility 무관 상호작용 가능</li>
 *   <li>viewer != owner (or anonymous) + PUBLIC → 상호작용 가능 (UNLISTED 포함)</li>
 *   <li>PRIVATE non-owner → 차단</li>
 *   <li>RESTRICTED non-owner → 차단 (legacy row 호환 — 신규 row는 차단됨)</li>
 *   <li>non-ACTIVE는 owner도 차단 (admin 우회 방지)</li>
 * </ul>
 *
 * <p>에러 매핑:
 * <ul>
 *   <li>레시피 없음 → RECIPE_NOT_FOUND (404)</li>
 *   <li>non-ACTIVE → RECIPE_NOT_FOUND (404, "존재하지 않는 것처럼" — V2 detail 가드 패턴 정합)</li>
 *   <li>PRIVATE/RESTRICTED non-owner → RECIPE_PRIVATE_ACCESS_DENIED (403, 명시적 차단)</li>
 * </ul>
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

        // V1.x 정책: viewableBy — link-only(PUBLIC+UNLISTED)도 상호작용 허용. listingStatus 무시.
        if (DevRecipeAccessPolicy.isViewableBy(recipe, viewerId)) {
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
