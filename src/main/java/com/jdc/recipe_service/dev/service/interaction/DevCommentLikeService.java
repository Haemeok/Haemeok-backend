package com.jdc.recipe_service.dev.service.interaction;

import com.jdc.recipe_service.domain.entity.RecipeComment;
import com.jdc.recipe_service.domain.repository.CommentLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeCommentRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.CommentLikeService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Dev V3 댓글 좋아요 dispatcher.
 *
 * 운영 {@link CommentLikeService#toggleLike}는 댓글이 달린 레시피 가시성 검증 없이 누구나 like 가능 → RESTRICTED/PRIVATE
 * 레시피 댓글에도 like 가능 + likeCount 노출 → "이 레시피 댓글이 존재한다" 누설 가능. dev V3는 comment → recipe 추적 후
 * 가시성 게이트 적용.
 *
 * 분기 (favorite 패턴 재사용 — comment-like는 알림 등 REQUIRES_NEW side effect 없으므로 outer @Transactional rollback이
 * 모든 변경을 안전하게 되돌림):
 *  - 기존 like 있음 → 게이트 우회, 운영 service 위임 (cleanup right)
 *  - 기존 like 없음 → comment → recipeId 추적 → validator 통과 후 운영 service 위임
 *  - race recovery: pre-check 우회 후 delegate가 add(true) 반환 시 post-check
 */
@Service
@RequiredArgsConstructor
public class DevCommentLikeService {

    private final DevRecipeAccessValidator accessValidator;
    private final CommentLikeRepository commentLikeRepository;
    private final RecipeCommentRepository recipeCommentRepository;
    private final CommentLikeService commentLikeService;

    /**
     * @return true = 좋아요 등록, false = 좋아요 취소
     */
    @Transactional
    public boolean toggleLike(Long userId, Long commentId) {
        boolean alreadyLiked = commentLikeRepository.existsByCommentIdAndUserId(commentId, userId);
        boolean checkedBeforeAdd = false;

        if (!alreadyLiked) {
            // 신규 add: comment → recipe 가시성 검증
            checkRecipeAccessForComment(commentId, userId);
            checkedBeforeAdd = true;
        }

        boolean liked = commentLikeService.toggleLike(commentId, userId);

        // race recovery: pre-check 우회 후 race add가 일어나도 outer @Transactional이 모든 변경 rollback
        // (REQUIRES_NEW side effect 없어 안전)
        if (liked && !checkedBeforeAdd) {
            checkRecipeAccessForComment(commentId, userId);
        }

        return liked;
    }

    @Transactional(readOnly = true)
    public int countLikes(Long commentId) {
        return commentLikeService.countLikes(commentId);
    }

    private void checkRecipeAccessForComment(Long commentId, Long userId) {
        RecipeComment comment = recipeCommentRepository.findById(commentId)
                .orElseThrow(() -> new CustomException(ErrorCode.COMMENT_NOT_FOUND));
        // comment.recipe는 @ManyToOne(LAZY)이지만 proxy가 FK를 이미 가짐 → getId()는 추가 SQL 없음
        Long recipeId = comment.getRecipe().getId();
        accessValidator.loadAndCheckInteractable(recipeId, userId);
    }
}
