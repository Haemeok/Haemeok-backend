package com.jdc.recipe_service.dev.controller.interaction;

import com.jdc.recipe_service.dev.service.interaction.DevCommentService;
import com.jdc.recipe_service.domain.dto.comment.CommentDto;
import com.jdc.recipe_service.domain.dto.comment.CommentRequestDto;
import com.jdc.recipe_service.domain.dto.comment.CommentWithRepliesDto;
import com.jdc.recipe_service.domain.dto.comment.ReplyDto;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.ResponseEntity;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevCommentController 단위 검증.
 *
 * 컨트롤러 책임: 인증 경계 + service 위임 + 응답 status. 게이트 분기는 DevCommentServiceTest가 잠근다.
 */
@ExtendWith(MockitoExtension.class)
class DevCommentControllerTest {

    @Mock DevCommentService devCommentService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;
    @Mock CommentDto commentDto;
    @Mock ReplyDto replyDto;
    @Mock CommentWithRepliesDto commentWithReplies;

    @InjectMocks DevCommentController controller;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;
    private static final Long PARENT_ID = 200L;
    private static final Long COMMENT_ID = 300L;
    private static final Pageable PAGE = PageRequest.of(0, 10);

    // ---------- POST create ----------

    @Test
    @DisplayName("create anonymous: UNAUTHORIZED + service 미호출")
    void create_anonymous_throwsUnauthorized() {
        CommentRequestDto req = CommentRequestDto.builder().content("hi").build();

        assertThatThrownBy(() -> controller.createComment(RECIPE_ID, req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devCommentService);
    }

    @Test
    @DisplayName("create authenticated: 201 Created + body")
    void create_authenticated_returns201() {
        CommentRequestDto req = CommentRequestDto.builder().content("nice").build();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCommentService.createComment(USER_ID, RECIPE_ID, req, user)).willReturn(commentDto);

        ResponseEntity<CommentDto> response = controller.createComment(RECIPE_ID, req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(201);
        assertThat(response.getBody()).isSameAs(commentDto);
    }

    // ---------- POST reply ----------

    @Test
    @DisplayName("reply anonymous: UNAUTHORIZED + service 미호출")
    void reply_anonymous_throwsUnauthorized() {
        CommentRequestDto req = CommentRequestDto.builder().content("re").build();

        assertThatThrownBy(() -> controller.createReply(RECIPE_ID, PARENT_ID, req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devCommentService);
    }

    @Test
    @DisplayName("reply authenticated: 201 Created + body")
    void reply_authenticated_returns201() {
        CommentRequestDto req = CommentRequestDto.builder().content("re").build();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCommentService.createReply(USER_ID, RECIPE_ID, PARENT_ID, req, user)).willReturn(replyDto);

        ResponseEntity<ReplyDto> response = controller.createReply(RECIPE_ID, PARENT_ID, req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(201);
        assertThat(response.getBody()).isSameAs(replyDto);
    }

    // ---------- GET list ----------

    @Test
    @DisplayName("list anonymous: viewerId=null로 service 호출 (게이트는 service 내부)")
    void list_anonymous_passesNullViewerId() {
        Page<CommentDto> page = new PageImpl<>(List.of(), PAGE, 0);
        given(devCommentService.getAllCommentsWithLikes(null, RECIPE_ID, PAGE)).willReturn(page);

        ResponseEntity<Page<CommentDto>> response = controller.getAllComments(RECIPE_ID, PAGE, null);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(page);
        verify(devCommentService).getAllCommentsWithLikes(null, RECIPE_ID, PAGE);
    }

    @Test
    @DisplayName("list authenticated: viewerId=user.id로 service 호출")
    void list_authenticated_passesViewerId() {
        Page<CommentDto> page = new PageImpl<>(List.of(commentDto), PAGE, 1);
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCommentService.getAllCommentsWithLikes(USER_ID, RECIPE_ID, PAGE)).willReturn(page);

        ResponseEntity<Page<CommentDto>> response = controller.getAllComments(RECIPE_ID, PAGE, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(page);
        verify(devCommentService).getAllCommentsWithLikes(USER_ID, RECIPE_ID, PAGE);
    }

    // ---------- GET replies ----------

    @Test
    @DisplayName("replies anonymous: viewerId=null로 service 호출")
    void replies_anonymous_passesNullViewerId() {
        given(devCommentService.getCommentWithReplies(null, RECIPE_ID, COMMENT_ID, PAGE)).willReturn(commentWithReplies);

        ResponseEntity<CommentWithRepliesDto> response = controller.getCommentWithReplies(RECIPE_ID, COMMENT_ID, PAGE, null);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(commentWithReplies);
    }

    @Test
    @DisplayName("replies authenticated: viewerId=user.id로 service 호출")
    void replies_authenticated_passesViewerId() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCommentService.getCommentWithReplies(USER_ID, RECIPE_ID, COMMENT_ID, PAGE)).willReturn(commentWithReplies);

        ResponseEntity<CommentWithRepliesDto> response = controller.getCommentWithReplies(RECIPE_ID, COMMENT_ID, PAGE, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(commentWithReplies);
    }

    // ---------- DELETE ----------

    @Test
    @DisplayName("delete anonymous: UNAUTHORIZED + service 미호출")
    void delete_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.deleteComment(COMMENT_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devCommentService);
    }

    @Test
    @DisplayName("delete authenticated: 200 OK + service 위임")
    void delete_authenticated_delegates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        ResponseEntity<String> response = controller.deleteComment(COMMENT_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isEqualTo("댓글이 삭제되었습니다.");
        verify(devCommentService).deleteComment(USER_ID, COMMENT_ID);
    }
}
