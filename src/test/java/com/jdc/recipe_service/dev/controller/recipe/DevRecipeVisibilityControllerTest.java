package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevVisibilityUpdateRequest;
import com.jdc.recipe_service.dev.domain.dto.recipe.DevVisibilityUpdateResponse;
import com.jdc.recipe_service.dev.service.recipe.DevRecipeVisibilityService;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;

/**
 * DevRecipeVisibilityController 단위 테스트.
 *
 * 검증:
 *  1. userDetails=null → UNAUTHORIZED throw, service 호출 안 됨
 *  2. 인증된 요청 → service에 (recipeId, visibility, user.id) 전달 + 200
 *  3. service exception 그대로 propagate (글로벌 핸들러가 매핑)
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeVisibilityControllerTest {

    @Mock DevRecipeVisibilityService devRecipeVisibilityService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevRecipeVisibilityController controller;

    private static final Long RECIPE_ID = 100L;
    private static final Long USER_ID = 1L;

    @Test
    @DisplayName("userDetails=null → UNAUTHORIZED throw, service 호출 안 됨")
    void anonymous_throwsUnauthorized() {
        DevVisibilityUpdateRequest req = new DevVisibilityUpdateRequest(RecipeVisibility.PRIVATE);

        assertThatThrownBy(() -> controller.updateVisibility(RECIPE_ID, req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verify(devRecipeVisibilityService, never()).updateVisibility(any(), any(), any());
    }

    @Test
    @DisplayName("인증된 요청: service에 (recipeId, visibility, userId) 전달 + 200")
    void authenticated_invokesServiceWithCorrectArgs() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        DevVisibilityUpdateRequest req = new DevVisibilityUpdateRequest(RecipeVisibility.PUBLIC);
        DevVisibilityUpdateResponse expected = new DevVisibilityUpdateResponse(
                RecipeVisibility.PUBLIC, false);
        given(devRecipeVisibilityService.updateVisibility(RECIPE_ID, RecipeVisibility.PUBLIC, USER_ID))
                .willReturn(expected);

        ResponseEntity<DevVisibilityUpdateResponse> response =
                controller.updateVisibility(RECIPE_ID, req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(expected);
        verify(devRecipeVisibilityService).updateVisibility(RECIPE_ID, RecipeVisibility.PUBLIC, USER_ID);
    }

    @Test
    @DisplayName("service의 RECIPE_ACCESS_DENIED 그대로 propagate (글로벌 핸들러가 403 매핑)")
    void serviceException_propagates() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        DevVisibilityUpdateRequest req = new DevVisibilityUpdateRequest(RecipeVisibility.PRIVATE);
        given(devRecipeVisibilityService.updateVisibility(RECIPE_ID, RecipeVisibility.PRIVATE, USER_ID))
                .willThrow(new CustomException(ErrorCode.RECIPE_ACCESS_DENIED));

        assertThatThrownBy(() -> controller.updateVisibility(RECIPE_ID, req, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);
    }

    @Test
    @DisplayName("RESTRICTED 요청: controller가 enum을 그대로 service에 전달 (API contract — 거부 정책은 service 책임)")
    void restrictedRequest_passedThroughToService() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);

        DevVisibilityUpdateRequest req = new DevVisibilityUpdateRequest(RecipeVisibility.RESTRICTED);
        // service가 INVALID_INPUT_VALUE를 던지는 동작은 DevRecipeVisibilityServiceTest가 검증.
        // 이 controller 테스트는 enum이 stripping 없이 service로 그대로 전달되는 contract를 잠근다.
        given(devRecipeVisibilityService.updateVisibility(RECIPE_ID, RecipeVisibility.RESTRICTED, USER_ID))
                .willThrow(new CustomException(ErrorCode.INVALID_INPUT_VALUE));

        // controller는 RESTRICTED를 거부하지 않고 그대로 전달 — 거부는 service의 단일 책임.
        // 이 테스트가 잡는 회귀: 누가 controller에 "RESTRICTED 일시 차단" 분기를 다시 넣으면 contract가 service와 갈려서
        // 실제 거부 메시지/에러코드가 두 군데에서 결정되는 위험이 생김.
        assertThatThrownBy(() -> controller.updateVisibility(RECIPE_ID, req, userDetails))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verify(devRecipeVisibilityService).updateVisibility(RECIPE_ID, RecipeVisibility.RESTRICTED, USER_ID);
    }
}
