package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.dev.policy.recipe.DevRecipeAccessPolicy;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjection;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjectionRepository;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeRemixQueryRepository;
import com.jdc.recipe_service.dev.service.interaction.DevRecipeAccessValidator;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
import com.jdc.recipe_service.service.RecipeRecommendationService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Dev V3 "이 레시피와 관련된 다른 레시피들" 조회 dispatcher.
 *
 * <h3>recommendations</h3>
 * <p>운영 {@link RecipeRecommendationService#getRecommendations}는 base recipe를 visibility 검증 없이 로드한다 (probing 가능).
 * PR 3 이후 후보 쿼리({@code findIdsByDishTypeIn}, {@code findRandomPublicRecipeIds})는 ACTIVE+PUBLIC+LISTED 4-enum 필터로
 * 좁혀졌지만, dev V3는 두 가지 보강을 더 한다:
 * <ol>
 *   <li>base recipe에 {@link DevRecipeAccessValidator} 게이트 (probing 차단)</li>
 *   <li>운영 service 결과에 batch access projection으로 post-filter — discovery 단일 정책({@code publicListedActive}) 미통과 ID 제외 +
 *       imageStatus PENDING/FAILED 제외 (다른 dev 카드 경로와 imageReady 컨벤션 정합)</li>
 * </ol>
 *
 * <p>post-filter 결과 {@code size} 미만일 수 있음을 수용 — 추천 알고리즘 (PAIRING_MAP, scoring) 복제 비용 회피.
 *
 * <h3>remixes</h3>
 * <p>운영 {@code RecipeRepository.findRemixesByOriginRecipeId}는 PR 3 이후 ACTIVE+PUBLIC+isPrivate=false 조건 (link-only 포함)을
 * 가지지만 {@code imageStatus} 필터가 없어 PENDING/FAILED remix 카드가 섞일 수 있다. dev V3는 {@link DevRecipeRemixQueryRepository}로
 * imageReady 정합 (READY OR NULL) 추가. base 원본은 validator 게이트.
 */
@Service
@RequiredArgsConstructor
public class DevRecipeRelatedService {

    private final DevRecipeAccessValidator accessValidator;
    private final DevRecipeAccessProjectionRepository accessProjectionRepository;
    private final RecipeRecommendationService recipeRecommendationService;
    private final DevRecipeRemixQueryRepository devRecipeRemixQueryRepository;

    @Transactional(readOnly = true)
    public List<RecipeSimpleStaticDto> getRecommendations(@Nullable Long viewerId, Long recipeId, int size) {
        accessValidator.loadAndCheckInteractable(recipeId, viewerId);

        List<RecipeSimpleStaticDto> raw = recipeRecommendationService.getRecommendations(recipeId, size);
        if (raw.isEmpty()) {
            return raw;
        }

        // post-filter: 운영 후보 쿼리는 PR 3 이후 ACTIVE+PUBLIC+LISTED 4-enum 필터를 가지지만 imageStatus 필터가 없음.
        // → 가시성(publicListedActive) + imageReady 둘 다 통과한 ID만 남김 (silent 제외 — dev V3 카드 정합)
        List<Long> ids = raw.stream().map(RecipeSimpleStaticDto::getId).toList();
        Set<Long> displayableIds = filterDisplayableIds(ids, viewerId);
        if (displayableIds.isEmpty()) {
            return Collections.emptyList();
        }

        return raw.stream()
                .filter(dto -> displayableIds.contains(dto.getId()))
                .toList();
    }

    @Transactional(readOnly = true)
    public Page<RecipeSimpleDto> findRemixes(@Nullable Long viewerId, Long originRecipeId, Pageable pageable) {
        accessValidator.loadAndCheckInteractable(originRecipeId, viewerId);
        // dev 전용 strict repo 사용 — imageReady 필터 추가됨
        return devRecipeRemixQueryRepository.findStrictRemixesByOriginRecipeId(originRecipeId, pageable);
    }

    /**
     * 추천 카드 후보 필터 — discovery 단일 정책 + 표시가능(imageReady) 둘 다 통과한 ID만 반환.
     *
     * <p>추천 카드는 base recipe의 owner라도 자신의 PRIVATE/UNLISTED 글이 추천 결과에 섞이면 안 된다
     * (link-only는 직접 링크로만 접근). 따라서 viewer 분기 없이 publicListedActive 단일 기준 — viewerId 인자는 무시.
     */
    private Set<Long> filterDisplayableIds(List<Long> recipeIds, @Nullable Long viewerId) {
        List<DevRecipeAccessProjection> projections =
                accessProjectionRepository.findAccessProjectionsByIds(recipeIds);
        return projections.stream()
                .filter(p -> DevRecipeAccessPolicy.isPublicListedActive(
                        p.lifecycleStatus(), p.visibility(), p.listingStatus()))
                .filter(DevRecipeAccessProjection::isImageReady)
                .map(DevRecipeAccessProjection::recipeId)
                .collect(Collectors.toSet());
    }
}
