package com.jdc.recipe_service.dev.controller.interaction;

import com.jdc.recipe_service.dev.service.interaction.DevInteractionService;
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
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevLikeFavoriteController 단위 검증.
 *
 * 컨트롤러 책임: 인증 경계 + service 위임 + 응답 shape. 게이트 분기 매트릭스는
 * {@link com.jdc.recipe_service.dev.service.interaction.DevInteractionServiceTest}가 잠근다.
 */
@ExtendWith(MockitoExtension.class)
class DevLikeFavoriteControllerTest {

    @Mock DevInteractionService devInteractionService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevLikeFavoriteController controller;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;

    // ---------- /{id}/like ----------

    @Test
    @DisplayName("like anonymous: UNAUTHORIZED + service 미호출")
    void like_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.toggleLike(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devInteractionService);
    }

    @Test
    @DisplayName("like authenticated + liked=true: 200 {liked=true, message=등록}")
    void like_authenticatedLiked_returnsTrueResponse() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devInteractionService.toggleLike(USER_ID, RECIPE_ID)).willReturn(true);

        ResponseEntity<Map<String, Object>> response = controller.toggleLike(RECIPE_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody())
                .containsEntry("liked", true)
                .containsEntry("message", "레시피 좋아요 등록 완료");
        verify(devInteractionService).toggleLike(USER_ID, RECIPE_ID);
    }

    @Test
    @DisplayName("like authenticated + liked=false: 200 {liked=false, message=취소}")
    void like_authenticatedUnliked_returnsFalseResponse() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devInteractionService.toggleLike(USER_ID, RECIPE_ID)).willReturn(false);

        ResponseEntity<Map<String, Object>> response = controller.toggleLike(RECIPE_ID, userDetails);

        assertThat(response.getBody())
                .containsEntry("liked", false)
                .containsEntry("message", "레시피 좋아요 취소 완료");
    }

    @Test
    @DisplayName("like: service throw → 그대로 propagate")
    void like_serviceThrows_propagates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(devInteractionService).toggleLike(USER_ID, RECIPE_ID);

        assertThatThrownBy(() -> controller.toggleLike(RECIPE_ID, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
    }

    // ---------- /{id}/favorite ----------

    @Test
    @DisplayName("favorite anonymous: UNAUTHORIZED + service 미호출")
    void favorite_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.toggleFavorite(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devInteractionService);
    }

    @Test
    @DisplayName("favorite authenticated + saved=true: 200 {saved=true, message=저장}")
    void favorite_authenticatedSaved_returnsTrueResponse() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devInteractionService.toggleFavorite(USER_ID, RECIPE_ID)).willReturn(true);

        ResponseEntity<Map<String, Object>> response = controller.toggleFavorite(RECIPE_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody())
                .containsEntry("saved", true)
                .containsEntry("message", "레시피 저장 완료");
        verify(devInteractionService).toggleFavorite(USER_ID, RECIPE_ID);
    }

    @Test
    @DisplayName("favorite authenticated + saved=false: 200 {saved=false, message=해제}")
    void favorite_authenticatedUnsaved_returnsFalseResponse() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devInteractionService.toggleFavorite(USER_ID, RECIPE_ID)).willReturn(false);

        ResponseEntity<Map<String, Object>> response = controller.toggleFavorite(RECIPE_ID, userDetails);

        assertThat(response.getBody())
                .containsEntry("saved", false)
                .containsEntry("message", "레시피 저장 해제 완료");
    }

    @Test
    @DisplayName("favorite: service throw → 그대로 propagate")
    void favorite_serviceThrows_propagates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(devInteractionService).toggleFavorite(USER_ID, RECIPE_ID);

        assertThatThrownBy(() -> controller.toggleFavorite(RECIPE_ID, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }
}
