package com.jdc.recipe_service.dev.service.interaction;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.domain.dto.comment.CommentWithRepliesDto;
import com.jdc.recipe_service.domain.dto.comment.ReplyDto;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.service.CommentService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Dev V3 댓글 dispatcher.
 *
 * 운영 댓글 API는 visibility 검증 없이 누구나 RESTRICTED/PRIVATE 레시피에 댓글/대댓글 작성 가능 (잠재 누수).
 * 운영 {@link CommentService#createComment}/{@link CommentService#createReply}는 댓글 저장 + 알림(REQUIRES_NEW) 발행 →
 * race로 게이트 우회 시 알림 누수 위험.
 *
 * dev V3 정책 (5 endpoint 모두 게이트, DELETE만 예외):
 *  - POST create/reply: {@link DevRecipeAccessValidator} 통과 → 운영 service. validator throw 시 operational 호출 안 함 → 알림 발생 자체 불가.
 *  - GET list/replies: 게이트 적용 — RESTRICTED 레시피 댓글 직접 URL 접근 차단 (dev V3 strictness).
 *  - DELETE: 게이트 미적용 — 본인 comment 정리는 recipe visibility 무관 (ownership 체크는 운영 service가 담당).
 *
 * Anonymous 처리:
 *  - GET endpoints: viewer null 허용 (validator는 null viewer를 PUBLIC+LISTED+ACTIVE만 통과시킴).
 *  - POST/DELETE: 컨트롤러에서 인증 강제 → service는 항상 비-null userId.
 */
@Service
@RequiredArgsConstructor
public class DevCommentService {

    private final DevRecipeAccessValidator accessValidator;
    private final CommentService commentService;

    @Transactional
    public CommentDto createComment(Long userId, Long recipeId, CommentRequestDto dto, User user) {
        accessValidator.loadAndCheckInteractable(recipeId, userId);
        return commentService.createComment(recipeId, dto, user);
    }

    @Transactional
    public ReplyDto createReply(Long userId, Long recipeId, Long parentId, CommentRequestDto dto, User user) {
        accessValidator.loadAndCheckInteractable(recipeId, userId);
        return commentService.createReply(recipeId, parentId, dto, user);
    }

    @Transactional(readOnly = true)
    public Page<CommentDto> getAllCommentsWithLikes(@Nullable Long viewerId, Long recipeId, Pageable pageable) {
        accessValidator.loadAndCheckInteractable(recipeId, viewerId);
        return commentService.getAllCommentsWithLikes(recipeId, viewerId, pageable);
    }

    @Transactional(readOnly = true)
    public CommentWithRepliesDto getCommentWithReplies(@Nullable Long viewerId, Long recipeId, Long commentId, Pageable pageable) {
        accessValidator.loadAndCheckInteractable(recipeId, viewerId);
        CommentDto parent = commentService.findByIdAndRecipeId(commentId, recipeId, viewerId);
        Page<ReplyDto> page = commentService.getRepliesWithLikes(commentId, viewerId, pageable);
        return new CommentWithRepliesDto(parent, page);
    }

    @Transactional
    public void deleteComment(Long userId, Long commentId) {
        // 게이트 없음 — cleanup right. 운영 service가 ownership(comment.user.id == userId) 체크 → COMMENT_ACCESS_DENIED.
        commentService.deleteComment(commentId, userId);
    }
}
