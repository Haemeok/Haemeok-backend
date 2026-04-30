package com.jdc.recipe_service.dev.controller.interaction;

import com.jdc.recipe_service.dev.service.interaction.DevRatingService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingRequestDto;
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
 * DevRecipeRatingController 단위 검증.
 *
 * 컨트롤러 책임: 인증 경계 + service 위임 + 응답 shape. 게이트 분기 매트릭스는 DevRatingServiceTest가 잠근다.
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeRatingControllerTest {

    @Mock DevRatingService devRatingService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevRecipeRatingController controller;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;

    // ---------- POST /{id} ----------

    @Test
    @DisplayName("rate anonymous: UNAUTHORIZED + service 미호출")
    void rate_anonymous_throwsUnauthorized() {
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(5.0).build();

        assertThatThrownBy(() -> controller.rateRecipe(RECIPE_ID, dto, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRatingService);
    }

    @Test
    @DisplayName("rate authenticated: service 위임 + 200 {message=등록}")
    void rate_authenticated_delegatesAndReturnsMessage() {
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(4.5).comment("good").build();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        ResponseEntity<Map<String, Object>> response = controller.rateRecipe(RECIPE_ID, dto, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).containsEntry("message", "평점 등록 완료");
        verify(devRatingService).rateRecipe(USER_ID, RECIPE_ID, dto);
    }

    @Test
    @DisplayName("rate: service throw → 그대로 propagate")
    void rate_serviceThrows_propagates() {
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(5.0).build();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(devRatingService).rateRecipe(USER_ID, RECIPE_ID, dto);

        assertThatThrownBy(() -> controller.rateRecipe(RECIPE_ID, dto, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
    }

    // ---------- GET /{id}/me ----------

    @Test
    @DisplayName("getMy anonymous: UNAUTHORIZED + service 미호출")
    void getMy_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.getMyRating(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRatingService);
    }

    @Test
    @DisplayName("getMy authenticated: service 위임 + 200 {rating: number}")
    void getMy_authenticated_returnsRating() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRatingService.getMyRating(USER_ID, RECIPE_ID)).willReturn(4.5);

        ResponseEntity<Map<String, Object>> response = controller.getMyRating(RECIPE_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).containsEntry("rating", 4.5);
    }

    @Test
    @DisplayName("getMy: rating 0.0도 정상 응답 (운영과 동일하게 unrated default)")
    void getMy_zeroRatingDefault() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRatingService.getMyRating(USER_ID, RECIPE_ID)).willReturn(0.0);

        ResponseEntity<Map<String, Object>> response = controller.getMyRating(RECIPE_ID, userDetails);

        assertThat(response.getBody()).containsEntry("rating", 0.0);
    }

    // ---------- DELETE /{id} ----------

    @Test
    @DisplayName("delete anonymous: UNAUTHORIZED + service 미호출")
    void delete_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.deleteRating(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRatingService);
    }

    @Test
    @DisplayName("delete authenticated: service 위임 + 200 {message=삭제}")
    void delete_authenticated_delegatesAndReturnsMessage() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        ResponseEntity<Map<String, Object>> response = controller.deleteRating(RECIPE_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).containsEntry("message", "평점 삭제 완료");
        verify(devRatingService).deleteRating(USER_ID, RECIPE_ID);
    }

    @Test
    @DisplayName("delete: service throw (RATING_NOT_FOUND) → 그대로 propagate")
    void delete_serviceThrows_propagates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RATING_NOT_FOUND))
                .given(devRatingService).deleteRating(USER_ID, RECIPE_ID);

        assertThatThrownBy(() -> controller.deleteRating(RECIPE_ID, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RATING_NOT_FOUND);
    }
}
