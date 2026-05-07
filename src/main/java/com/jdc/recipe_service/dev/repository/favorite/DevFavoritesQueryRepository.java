package com.jdc.recipe_service.dev.repository.favorite;

import com.jdc.recipe_service.domain.entity.Recipe;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

/**
 * Dev V3 favorites query repository.
 *
 * <p>운영 {@code RecipeFavoriteRepository.findMyFavoritesWithPending}의 dev 미러:
 * <ul>
 *   <li>운영의 {@code (r.isPrivate = false OR r.imageStatus = 'PENDING')} 자리에 V1.x 정책({@code viewableBy(viewerId)})
 *       + imageReady (READY/null만, PENDING 차단 — A2/A3 정합)</li>
 *   <li>viewer == favoriter == 같은 사용자 — 즐겨찾기는 본인 컬렉션</li>
 *   <li>자신이 만든 PRIVATE은 즐겨찾기에 포함되어도 노출. 다른 사람 PUBLIC(LISTED+UNLISTED 모두) 노출 — link-only도 저장된 상태면 보임.
 *       다른 사람 PRIVATE/RESTRICTED는 차단</li>
 * </ul>
 *
 * <p>운영보다 더 엄격: 운영은 PENDING 노출(AI 생성 중 즐겨찾기 가시성)이지만 dev는 imageReady 통일.
 * 다른 사람 RESTRICTED는 운영(isPrivate=false라 노출)과 달리 dev는 차단(viewableBy 정책).
 *
 * <p>Recipe entity Slice → service에서 DTO 변환 (likedByCurrentUser batch 등).
 */
public interface DevFavoritesQueryRepository {

    /**
     * userId가 즐겨찾기한 레시피 중 dev 정책 통과한 것만 페이지.
     * (favoriter == viewer 가정 — 즐겨찾기는 본인 컬렉션이라 다른 사람이 본 적 없음.)
     */
    Page<Recipe> findFavoritesAccessible(Long userId, Pageable pageable);
}
