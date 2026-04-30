package com.jdc.recipe_service.dev.service.interaction;

import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.domain.dto.comment.CommentWithRepliesDto;
import com.jdc.recipe_service.domain.dto.comment.ReplyDto;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.CommentService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevCommentService 분기 매트릭스.
 *
 *  - POST create/reply: validator 통과 시 운영 service 위임 (알림은 게이트 통과한 add에만)
 *  - POST create/reply: validator throw 시 운영 service 미호출 (알림 누수 차단)
 *  - GET list/replies: validator 적용 — anonymous는 PUBLIC+LISTED+ACTIVE만, RESTRICTED non-owner 차단
 *  - DELETE: 게이트 없음 — 운영 service에 단순 위임 (ownership 체크는 운영이 담당)
 */
@ExtendWith(MockitoExtension.class)
class DevCommentServiceTest {

    @Mock DevRecipeAccessValidator accessValidator;
    @Mock CommentService commentService;
    @Mock User user;
    @Mock CommentDto commentDto;
    @Mock ReplyDto replyDto;

    @InjectMocks DevCommentService devCommentService;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;
    private static final Long PARENT_COMMENT_ID = 200L;
    private static final Long COMMENT_ID = 300L;
    private static final Pageable PAGE = PageRequest.of(0, 10);

    // ---------- createComment ----------

    @Test
    @DisplayName("[create] validator 통과 → 운영 commentService에 위임 (알림 포함)")
    void create_validatorPass_delegates() {
        CommentRequestDto dto = CommentRequestDto.builder().content("nice").build();
        given(commentService.createComment(RECIPE_ID, dto, user)).willReturn(commentDto);

        CommentDto result = devCommentService.createComment(USER_ID, RECIPE_ID, dto, user);

        assertThat(result).isSameAs(commentDto);
        InOrder order = inOrder(accessValidator, commentService);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        order.verify(commentService).createComment(RECIPE_ID, dto, user);
    }

    @Test
    @DisplayName("[create] validator throw → commentService 미호출 (알림 누수 차단)")
    void create_validatorThrow_skipsService() {
        CommentRequestDto dto = CommentRequestDto.builder().content("nope").build();
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devCommentService.createComment(USER_ID, RECIPE_ID, dto, user))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(commentService);
    }

    // ---------- createReply ----------

    @Test
    @DisplayName("[reply] validator 통과 → 운영 commentService.createReply 위임 (대댓글 알림 포함)")
    void reply_validatorPass_delegates() {
        CommentRequestDto dto = CommentRequestDto.builder().content("re: nice").build();
        given(commentService.createReply(RECIPE_ID, PARENT_COMMENT_ID, dto, user)).willReturn(replyDto);

        ReplyDto result = devCommentService.createReply(USER_ID, RECIPE_ID, PARENT_COMMENT_ID, dto, user);

        assertThat(result).isSameAs(replyDto);
        InOrder order = inOrder(accessValidator, commentService);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        order.verify(commentService).createReply(RECIPE_ID, PARENT_COMMENT_ID, dto, user);
    }

    @Test
    @DisplayName("[reply] validator throw → createReply 미호출")
    void reply_validatorThrow_skipsService() {
        CommentRequestDto dto = CommentRequestDto.builder().content("blocked").build();
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devCommentService.createReply(USER_ID, RECIPE_ID, PARENT_COMMENT_ID, dto, user))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        verifyNoInteractions(commentService);
    }

    // ---------- getAllCommentsWithLikes ----------

    @Test
    @DisplayName("[list] validator 통과 → 운영 commentService.getAllCommentsWithLikes 위임")
    void list_validatorPass_delegates() {
        Page<CommentDto> page = new PageImpl<>(List.of(commentDto), PAGE, 1);
        given(commentService.getAllCommentsWithLikes(eq(RECIPE_ID), eq(USER_ID), eq(PAGE))).willReturn(page);

        Page<CommentDto> result = devCommentService.getAllCommentsWithLikes(USER_ID, RECIPE_ID, PAGE);

        assertThat(result).isSameAs(page);
        verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
    }

    @Test
    @DisplayName("[list] anonymous (viewerId=null) + validator 통과 (PUBLIC+LISTED+ACTIVE) → 위임")
    void list_anonymousValidatorPass_delegates() {
        Page<CommentDto> page = new PageImpl<>(List.of(), PAGE, 0);
        given(commentService.getAllCommentsWithLikes(eq(RECIPE_ID), eq(null), eq(PAGE))).willReturn(page);

        Page<CommentDto> result = devCommentService.getAllCommentsWithLikes(null, RECIPE_ID, PAGE);

        assertThat(result).isSameAs(page);
        verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, null);
    }

    @Test
    @DisplayName("[list] validator throw (RESTRICTED non-owner) → commentService 미호출")
    void list_validatorThrow_skipsService() {
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devCommentService.getAllCommentsWithLikes(USER_ID, RECIPE_ID, PAGE))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(commentService);
    }

    // ---------- getCommentWithReplies ----------

    @Test
    @DisplayName("[replies] validator 통과 → parent + page<reply> 합쳐서 반환")
    void replies_validatorPass_combinesParentAndPage() {
        Page<ReplyDto> replyPage = new PageImpl<>(List.of(replyDto), PAGE, 1);
        given(commentService.findByIdAndRecipeId(COMMENT_ID, RECIPE_ID, USER_ID)).willReturn(commentDto);
        given(commentService.getRepliesWithLikes(COMMENT_ID, USER_ID, PAGE)).willReturn(replyPage);

        CommentWithRepliesDto result = devCommentService.getCommentWithReplies(USER_ID, RECIPE_ID, COMMENT_ID, PAGE);

        assertThat(result).isNotNull();
        assertThat(result.getParentComment()).isSameAs(commentDto);
        assertThat(result.getReplies()).isSameAs(replyPage);
        verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
    }

    @Test
    @DisplayName("[replies] validator throw → parent/replies 모두 조회 안 함")
    void replies_validatorThrow_skipsAllReads() {
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devCommentService.getCommentWithReplies(USER_ID, RECIPE_ID, COMMENT_ID, PAGE))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        verifyNoInteractions(commentService);
    }

    // ---------- deleteComment ----------

    @Test
    @DisplayName("[delete] 게이트 없이 운영 commentService.deleteComment에 위임 (cleanup right)")
    void delete_noGateDelegates() {
        devCommentService.deleteComment(USER_ID, COMMENT_ID);

        verify(commentService).deleteComment(COMMENT_ID, USER_ID);
        verifyNoInteractions(accessValidator);
    }

    @Test
    @DisplayName("[delete] 운영 service throw (ownership/not-found) → 그대로 propagate")
    void delete_propagatesAccessDenied() {
        willThrow(new CustomException(ErrorCode.COMMENT_ACCESS_DENIED))
                .given(commentService).deleteComment(COMMENT_ID, USER_ID);

        assertThatThrownBy(() -> devCommentService.deleteComment(USER_ID, COMMENT_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.COMMENT_ACCESS_DENIED);
    }
}
