package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.dev.service.recipe.DevRecipeImageWriteService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeImageKeyUpdateRequest;
import com.jdc.recipe_service.domain.dto.url.FinalizeResponse;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlRequest;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlResponse;
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

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeImageWriteController 단위 검증.
 *
 * 컨트롤러 책임: 인증 경계, service 위임, 응답 status. 게이트는 service test가 잠근다.
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeImageWriteControllerTest {

    @Mock DevRecipeImageWriteService devRecipeImageWriteService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;
    @Mock UpdatePresignedUrlResponse presignedResponse;
    @Mock FinalizeResponse finalizeResponse;

    @InjectMocks DevRecipeImageWriteController controller;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;

    // ---------- presigned-urls ----------

    @Test
    @DisplayName("presigned anonymous: UNAUTHORIZED + service 미호출")
    void presigned_anonymous_throwsUnauthorized() {
        UpdatePresignedUrlRequest req = UpdatePresignedUrlRequest.builder().build();

        assertThatThrownBy(() -> controller.getPresignedUrlsForUpdate(RECIPE_ID, req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRecipeImageWriteService);
    }

    @Test
    @DisplayName("presigned authenticated: 200 + service 위임")
    void presigned_authenticated_returns200() {
        UpdatePresignedUrlRequest req = UpdatePresignedUrlRequest.builder().files(List.of()).build();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRecipeImageWriteService.generatePresignedUrlsForUpdate(eq(USER_ID), eq(RECIPE_ID), any()))
                .willReturn(presignedResponse);

        ResponseEntity<UpdatePresignedUrlResponse> response = controller.getPresignedUrlsForUpdate(RECIPE_ID, req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(presignedResponse);
    }

    @Test
    @DisplayName("presigned: request body가 null이어도 service에는 null files로 위임 (service가 처리)")
    void presigned_nullRequestBody_passesNullFiles() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRecipeImageWriteService.generatePresignedUrlsForUpdate(eq(USER_ID), eq(RECIPE_ID), eq(null)))
                .willReturn(presignedResponse);

        controller.getPresignedUrlsForUpdate(RECIPE_ID, null, userDetails);

        verify(devRecipeImageWriteService).generatePresignedUrlsForUpdate(USER_ID, RECIPE_ID, null);
    }

    @Test
    @DisplayName("presigned: service throw → propagate")
    void presigned_serviceThrows_propagates() {
        UpdatePresignedUrlRequest req = UpdatePresignedUrlRequest.builder().files(List.of()).build();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_ACCESS_DENIED))
                .given(devRecipeImageWriteService).generatePresignedUrlsForUpdate(eq(USER_ID), eq(RECIPE_ID), any());

        assertThatThrownBy(() -> controller.getPresignedUrlsForUpdate(RECIPE_ID, req, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);
    }

    // ---------- finalize ----------

    @Test
    @DisplayName("finalize anonymous: UNAUTHORIZED + service 미호출")
    void finalize_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.finalizeRecipeImages(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRecipeImageWriteService);
    }

    @Test
    @DisplayName("finalize authenticated: 200 + service 위임")
    void finalize_authenticated_returns200() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRecipeImageWriteService.finalizeRecipeImages(USER_ID, RECIPE_ID)).willReturn(finalizeResponse);

        ResponseEntity<FinalizeResponse> response = controller.finalizeRecipeImages(RECIPE_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(finalizeResponse);
    }

    @Test
    @DisplayName("finalize: service throw → propagate")
    void finalize_serviceThrows_propagates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(devRecipeImageWriteService).finalizeRecipeImages(USER_ID, RECIPE_ID);

        assertThatThrownBy(() -> controller.finalizeRecipeImages(RECIPE_ID, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    // ---------- updateImageKeys ----------

    @Test
    @DisplayName("updateImageKeys anonymous: UNAUTHORIZED + service 미호출")
    void updateImageKeys_anonymous_throwsUnauthorized() {
        RecipeImageKeyUpdateRequest req = new RecipeImageKeyUpdateRequest();

        assertThatThrownBy(() -> controller.updateRecipeImageKeys(RECIPE_ID, req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRecipeImageWriteService);
    }

    @Test
    @DisplayName("updateImageKeys authenticated: 200 (응답 body 없음) + service 위임")
    void updateImageKeys_authenticated_returns200NoBody() {
        RecipeImageKeyUpdateRequest req = new RecipeImageKeyUpdateRequest();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        ResponseEntity<Void> response = controller.updateRecipeImageKeys(RECIPE_ID, req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isNull();
        verify(devRecipeImageWriteService).updateImageKeys(USER_ID, RECIPE_ID, req);
    }

    @Test
    @DisplayName("updateImageKeys: service throw (USER_RECIPE_IMAGE_REQUIRED) → propagate")
    void updateImageKeys_serviceThrows_propagates() {
        RecipeImageKeyUpdateRequest req = new RecipeImageKeyUpdateRequest();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.USER_RECIPE_IMAGE_REQUIRED))
                .given(devRecipeImageWriteService).updateImageKeys(USER_ID, RECIPE_ID, req);

        assertThatThrownBy(() -> controller.updateRecipeImageKeys(RECIPE_ID, req, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.USER_RECIPE_IMAGE_REQUIRED);
    }
}
