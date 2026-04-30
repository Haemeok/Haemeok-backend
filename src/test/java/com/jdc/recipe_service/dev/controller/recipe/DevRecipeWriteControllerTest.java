package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.dev.service.recipe.DevRecipeWriteService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeUpdateWithImageRequest;
import com.jdc.recipe_service.domain.dto.recipe.RecipeWithImageUploadRequest;
import com.jdc.recipe_service.domain.dto.url.PresignedUrlResponse;
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeWriteController 단위 검증.
 *
 * 컨트롤러 책임: 인증 경계, service 위임, 응답 status (201/200). 게이트는 service test가 잠근다.
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeWriteControllerTest {

    @Mock DevRecipeWriteService devRecipeWriteService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;
    @Mock PresignedUrlResponse presignedResponse;

    @InjectMocks DevRecipeWriteController controller;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;

    // ---------- POST / ----------

    @Test
    @DisplayName("create anonymous: UNAUTHORIZED + service 미호출")
    void create_anonymous_throwsUnauthorized() {
        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder().build();

        assertThatThrownBy(() -> controller.createRecipe(req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRecipeWriteService);
    }

    @Test
    @DisplayName("create authenticated: 201 Created + body")
    void create_authenticated_returns201() {
        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder().build();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRecipeWriteService.createRecipe(USER_ID, req)).willReturn(presignedResponse);

        ResponseEntity<PresignedUrlResponse> response = controller.createRecipe(req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(201);
        assertThat(response.getBody()).isSameAs(presignedResponse);
    }

    @Test
    @DisplayName("create: service throw → propagate")
    void create_serviceThrows_propagates() {
        RecipeWithImageUploadRequest req = RecipeWithImageUploadRequest.builder().build();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_REMIX_NOT_ALLOWED))
                .given(devRecipeWriteService).createRecipe(USER_ID, req);

        assertThatThrownBy(() -> controller.createRecipe(req, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_REMIX_NOT_ALLOWED);
    }

    // ---------- PUT /{id} ----------

    @Test
    @DisplayName("update anonymous: UNAUTHORIZED + service 미호출")
    void update_anonymous_throwsUnauthorized() {
        RecipeUpdateWithImageRequest req = RecipeUpdateWithImageRequest.builder().build();

        assertThatThrownBy(() -> controller.updateRecipe(RECIPE_ID, req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRecipeWriteService);
    }

    @Test
    @DisplayName("update authenticated: 200 OK + service 위임")
    void update_authenticated_returns200() {
        RecipeUpdateWithImageRequest req = RecipeUpdateWithImageRequest.builder().build();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRecipeWriteService.updateRecipe(USER_ID, RECIPE_ID, req)).willReturn(presignedResponse);

        ResponseEntity<PresignedUrlResponse> response = controller.updateRecipe(RECIPE_ID, req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(presignedResponse);
        verify(devRecipeWriteService).updateRecipe(USER_ID, RECIPE_ID, req);
    }

    @Test
    @DisplayName("update: service throw (NOT_FOUND/ACCESS_DENIED) → propagate")
    void update_serviceThrows_propagates() {
        RecipeUpdateWithImageRequest req = RecipeUpdateWithImageRequest.builder().build();
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(devRecipeWriteService).updateRecipe(USER_ID, RECIPE_ID, req);

        assertThatThrownBy(() -> controller.updateRecipe(RECIPE_ID, req, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    // ---------- DELETE /{id} ----------

    @Test
    @DisplayName("delete anonymous: UNAUTHORIZED + service 미호출")
    void delete_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.deleteRecipe(RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRecipeWriteService);
    }

    @Test
    @DisplayName("delete authenticated: 200 OK + service 위임 + 메시지 응답")
    void delete_authenticated_returns200WithMessage() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        ResponseEntity<String> response = controller.deleteRecipe(RECIPE_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isEqualTo("레시피가 성공적으로 삭제되었습니다.");
        verify(devRecipeWriteService).deleteRecipe(USER_ID, RECIPE_ID);
    }

    @Test
    @DisplayName("delete: service throw (RECIPE_ACCESS_DENIED) → propagate")
    void delete_serviceThrows_propagates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_ACCESS_DENIED))
                .given(devRecipeWriteService).deleteRecipe(USER_ID, RECIPE_ID);

        assertThatThrownBy(() -> controller.deleteRecipe(RECIPE_ID, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);
    }
}
