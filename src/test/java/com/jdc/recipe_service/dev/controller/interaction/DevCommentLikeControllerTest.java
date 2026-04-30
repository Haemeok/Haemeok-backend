package com.jdc.recipe_service.dev.controller.interaction;

import com.jdc.recipe_service.dev.service.interaction.DevCommentLikeService;
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
import org.springframework.http.ResponseEntity;

import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevCommentLikeController 단위 검증.
 *
 * 컨트롤러 책임: 인증 경계, service 위임, 응답 shape ({liked, likeCount, message}).
 * 게이트 분기는 DevCommentLikeServiceTest가 잠근다.
 */
@ExtendWith(MockitoExtension.class)
class DevCommentLikeControllerTest {

    @Mock DevCommentLikeService devCommentLikeService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevCommentLikeController controller;

    private static final Long USER_ID = 7L;
    private static final Long COMMENT_ID = 200L;

    @Test
    @DisplayName("anonymous: UNAUTHORIZED + service 미호출")
    void anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.toggleLike(COMMENT_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devCommentLikeService);
    }

    @Test
    @DisplayName("authenticated + liked=true: 200 {liked=true, likeCount, message=등록}")
    void authenticatedLiked_returnsTrueResponse() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCommentLikeService.toggleLike(USER_ID, COMMENT_ID)).willReturn(true);
        given(devCommentLikeService.countLikes(COMMENT_ID)).willReturn(5);

        ResponseEntity<Map<String, Object>> response = controller.toggleLike(COMMENT_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody())
                .containsEntry("liked", true)
                .containsEntry("likeCount", 5)
                .containsEntry("message", "댓글 좋아요 등록 완료");
    }

    @Test
    @DisplayName("authenticated + liked=false: 200 {liked=false, likeCount, message=취소}")
    void authenticatedUnliked_returnsFalseResponse() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devCommentLikeService.toggleLike(USER_ID, COMMENT_ID)).willReturn(false);
        given(devCommentLikeService.countLikes(COMMENT_ID)).willReturn(4);

        ResponseEntity<Map<String, Object>> response = controller.toggleLike(COMMENT_ID, userDetails);

        assertThat(response.getBody())
                .containsEntry("liked", false)
                .containsEntry("likeCount", 4)
                .containsEntry("message", "댓글 좋아요 취소 완료");
    }

    @Test
    @DisplayName("service throw → 그대로 propagate (countLikes 미호출)")
    void serviceThrows_propagates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(devCommentLikeService).toggleLike(USER_ID, COMMENT_ID);

        assertThatThrownBy(() -> controller.toggleLike(COMMENT_ID, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        // toggleLike가 throw하면 countLikes는 호출되지 않아야 함 — 차단된 댓글의 likeCount 노출 방지
        verify(devCommentLikeService, never()).countLikes(COMMENT_ID);
    }
}
