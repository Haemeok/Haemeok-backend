package com.jdc.recipe_service.dev.service.interaction;

import com.jdc.recipe_service.domain.entity.RecipeLike;
import com.jdc.recipe_service.domain.repository.RecipeBookItemRepository;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.service.RecipeBookService;
import com.jdc.recipe_service.service.RecipeLikeService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

/**
 * Dev V3 like/favorite 토글 dispatcher.
 *
 * 게이트는 "신규 추가 경로"에만 적용. 기존 interaction 제거는 항상 허용 — 사용자가 과거에 like/favorite한 뒤 레시피가
 * RESTRICTED/PRIVATE/non-ACTIVE로 바뀌어도 본인 정리 권한은 보장되어야 한다.
 *
 * <h3>like 경로: 분기 자체로 race add 차단</h3>
 * 운영 {@link RecipeLikeService#toggleLike}는 add 경로에서 {@link com.jdc.recipe_service.service.NotificationService}
 * (REQUIRES_NEW)로 알림 + WebSocket을 발행한다. outer {@code @Transactional} rollback에 묶이지 않으므로 race로 add가
 * 일어난 뒤 post-check throw로 like row를 rollback해도 알림 row + 이벤트는 새어나간다.
 *
 * 따라서 like는 운영 service를 "토글"로 호출하지 않고 분기로 처리한다:
 *  - {@code alreadyLiked=true} → remove-only 직접 처리 (race로 이미 사라졌어도 절대 add하지 않음)
 *  - {@code alreadyLiked=false} → {@link DevRecipeAccessValidator} 통과 후에만 운영 service 위임 (정상 add 경로)
 *
 * <h3>favorite 경로: post-check race recovery</h3>
 * 운영 {@link RecipeBookService#toggleSave}는 REQUIRES_NEW side effect가 없어 outer transaction rollback이 모든 변경을
 * 되돌린다. 따라서 pre-check + delegate + post-check 패턴으로 충분.
 *
 * <p><b>운영 코드 동기화 주의:</b> 향후 {@link RecipeLikeService#toggleLike} remove 경로에 추가 side effect가 들어가면
 * 이 클래스의 {@code removeLikeOnly} 도 함께 갱신해야 한다.
 *
 * Source of truth:
 *  - like: {@link RecipeLikeRepository#existsByRecipeIdAndUserId}
 *  - favorite: {@link RecipeBookItemRepository#existsByUserIdAndRecipeId} — 운영 {@link RecipeBookService#toggleSave}
 *    기준과 동일 (legacy {@code recipe_favorites}가 아닌 {@code recipe_book_items})
 */
@Service
@RequiredArgsConstructor
public class DevInteractionService {

    private final DevRecipeAccessValidator accessValidator;
    private final RecipeLikeRepository likeRepository;
    private final RecipeRepository recipeRepository;
    private final RecipeBookItemRepository bookItemRepository;
    private final RecipeLikeService likeService;
    private final RecipeBookService recipeBookService;

    /**
     * @return true = 좋아요 등록, false = 좋아요 취소
     */
    @Transactional
    public boolean toggleLike(Long userId, Long recipeId) {
        boolean alreadyLiked = likeRepository.existsByRecipeIdAndUserId(recipeId, userId);

        if (alreadyLiked) {
            // remove-only: race로 이미 사라졌어도 add 절대 안 함 → 게이트 우회 add 누수 원천 차단
            return removeLikeOnly(userId, recipeId);
        }

        // 신규 추가: 게이트 통과 후에만 운영 service 호출 (알림은 게이트 통과한 add에만 발생)
        accessValidator.loadAndCheckInteractable(recipeId, userId);
        return likeService.toggleLike(userId, recipeId);
    }

    private boolean removeLikeOnly(Long userId, Long recipeId) {
        Optional<RecipeLike> like = likeRepository.findByUserIdAndRecipeId(userId, recipeId);
        if (like.isPresent()) {
            likeRepository.delete(like.get());
            recipeRepository.decrementLikeCount(recipeId);
        }
        // race로 이미 삭제됐으면 no-op. 어쨌든 최종 상태는 "안 좋아함" → false.
        return false;
    }

    /**
     * @return true = 저장 등록, false = 저장 해제
     */
    @Transactional
    public boolean toggleFavorite(Long userId, Long recipeId) {
        boolean alreadySaved = bookItemRepository.existsByUserIdAndRecipeId(userId, recipeId);
        boolean checkedBeforeAdd = false;

        if (!alreadySaved) {
            accessValidator.loadAndCheckInteractable(recipeId, userId);
            checkedBeforeAdd = true;
        }

        boolean saved = recipeBookService.toggleSave(userId, recipeId);

        // race recovery: pre-check 우회 후 race add가 일어나도 outer @Transactional이 모든 변경을 rollback
        // (favorite은 REQUIRES_NEW side effect 없어 안전)
        if (saved && !checkedBeforeAdd) {
            accessValidator.loadAndCheckInteractable(recipeId, userId);
        }

        return saved;
    }
}
