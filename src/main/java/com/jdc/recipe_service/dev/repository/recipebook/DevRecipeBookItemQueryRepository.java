package com.jdc.recipe_service.dev.repository.recipebook;

import com.jdc.recipe_service.domain.entity.RecipeBookItem;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;

import java.util.List;
import java.util.Map;

/**
 * Dev V3 레시피북 아이템 query repository.
 *
 * <p>운영 {@code RecipeBookItemRepository}의 3개 accessible 쿼리를 dev V1.x 정책으로 미러:
 * <ul>
 *   <li>운영의 {@code (r.isPrivate = false OR r.user.id = :userId)} 자리에 {@code viewableBy(viewerId)}</li>
 *   <li>lifecycleStatus=ACTIVE + (owner OR PUBLIC) — 다른 사람 PRIVATE/RESTRICTED는 차단.
 *       link-only(PUBLIC+UNLISTED)도 폴더에 저장돼 있으면 노출 (listingStatus 무시)</li>
 *   <li>imageReady (READY/null) 추가 — 운영 미적용이지만 dev V3 일관 정책</li>
 * </ul>
 *
 * <p>userId == 폴더 소유자 == viewer (book 자체가 사용자 본인 컬렉션).
 */
public interface DevRecipeBookItemQueryRepository {

    /**
     * book 안의 dev 정책 통과 아이템 페이지.
     * Recipe + recipe.user fetch join — N+1 차단.
     */
    Slice<RecipeBookItem> findAccessibleDevByBookIdAndUserId(Long bookId, Long userId, Pageable pageable);

    /** book detail의 recipeCount 계산용 — dev 정책 통과 아이템만. */
    int countAccessibleDevByBookIdAndUserId(Long bookId, Long userId);

    /**
     * book 목록의 count 집계용 — bookId → 정책 통과 아이템 수.
     * userId 소유의 모든 book에 대해 한 번에 집계.
     */
    Map<Long, Integer> countAccessibleDevByUserIdGroupByBookId(Long userId);
}
