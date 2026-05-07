package com.jdc.recipe_service.dev.service.status;

import com.jdc.recipe_service.dev.policy.recipe.DevRecipeAccessPolicy;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjection;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjectionRepository;
import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeSaveStatusResponse;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeDetailStatusDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStatusDto;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeBookService;
import com.jdc.recipe_service.service.RecipeStatusService;
import lombok.RequiredArgsConstructor;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Dev V3 status/check dispatcher.
 *
 * Interaction phase가 RESTRICTED 레시피 상호작용을 막았지만 운영 {@link RecipeStatusService}는 가시성 검증 없이
 * 본인 상태(좋아요/즐겨찾기/평점/댓글) 조회를 그대로 허용 → "내가 좋아요 했는지" 같은 status 조회로 RESTRICTED
 * 레시피 존재 + 본인 상태 누설 가능. dev V3는 status 진입에서도 가시성 게이트 적용.
 *
 * 분기:
 *  - **단건 status**: {@link DevRecipeAccessValidator}로 게이트. 차단 시 403/404 throw.
 *  - **배치 status**: 입력 ID들을 사전 access projection으로 필터 → 접근 가능한 ID만 운영 service에 전달.
 *    응답 Map shape이 partial result를 자연스럽게 표현 (없는 ID = 접근 불가). per-id throw 안 함.
 *  - **saved-books**: 본인 collection 메타. 게이트 없음 — 입력 recipeId는 호출자가 이미 가진 정보, 응답은
 *    본인 폴더 정보만 담음 → 새 recipe 정보 누설 없음. 단순 위임.
 */
@Service
@RequiredArgsConstructor
public class DevRecipeStatusService {

    private final DevRecipeAccessValidator accessValidator;
    private final DevRecipeAccessProjectionRepository accessProjectionRepository;
    private final RecipeStatusService recipeStatusService;
    private final RecipeBookService recipeBookService;

    /**
     * 단건 status — 게이트 throw 시 propagate.
     */
    @Transactional(readOnly = true)
    public RecipeDetailStatusDto getDetailStatus(@Nullable Long viewerId, Long recipeId) {
        accessValidator.loadAndCheckInteractable(recipeId, viewerId);

        Map<Long, RecipeDetailStatusDto> statuses =
                recipeStatusService.getStatuses(Collections.singletonList(recipeId), viewerId);
        RecipeDetailStatusDto detail = statuses.get(recipeId);
        if (detail == null) {
            throw new CustomException(ErrorCode.RECIPE_NOT_FOUND);
        }
        return detail;
    }

    /**
     * 배치 status — 접근 가능한 ID만 결과에 포함 (silent filter).
     * @return Map<rawRecipeId, dto> — encoding은 컨트롤러가 담당
     */
    @Transactional(readOnly = true)
    public Map<Long, RecipeSimpleStatusDto> getBatchSimpleStatuses(@Nullable Long viewerId, List<Long> recipeIds) {
        if (recipeIds == null || recipeIds.isEmpty()) {
            return Collections.emptyMap();
        }

        Set<Long> accessibleIds = filterAccessibleIds(recipeIds, viewerId);
        if (accessibleIds.isEmpty()) {
            return Collections.emptyMap();
        }

        List<Long> filtered = recipeIds.stream()
                .filter(accessibleIds::contains)
                .toList();

        Map<Long, RecipeDetailStatusDto> details = recipeStatusService.getStatuses(filtered, viewerId);
        return recipeStatusService.convertToSimpleStatus(details);
    }

    /**
     * 본인 폴더 저장 상태 — 게이트 없음, 단순 위임.
     */
    @Transactional(readOnly = true)
    public RecipeSaveStatusResponse getSaveStatus(Long userId, Long recipeId) {
        return recipeBookService.getSaveStatus(userId, recipeId);
    }

    private Set<Long> filterAccessibleIds(List<Long> recipeIds, @Nullable Long viewerId) {
        List<DevRecipeAccessProjection> projections =
                accessProjectionRepository.findAccessProjectionsByIds(recipeIds);
        return projections.stream()
                // V1.x 정책: status 조회는 viewable 단위 — PUBLIC+UNLISTED도 동적 상태 조회 가능. listingStatus 무시.
                .filter(p -> DevRecipeAccessPolicy.isViewableBy(
                        p.lifecycleStatus(), p.visibility(), viewerId, p.ownerId()))
                .map(DevRecipeAccessProjection::recipeId)
                .collect(Collectors.toSet());
    }
}
