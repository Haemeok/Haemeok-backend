package com.jdc.recipe_service.dev.controller.recipe;

import com.jdc.recipe_service.dev.service.recipe.DevRecipeRelatedService;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.dto.v2.recipe.RecipeSimpleStaticDto;
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
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.verify;

/**
 * DevRecipeRelatedController 단위 검증.
 *
 * 컨트롤러 책임: viewerId 추출 (anonymous=null), service 위임, 응답 status. 게이트는 service test가 잠근다.
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeRelatedControllerTest {

    @Mock DevRecipeRelatedService devRecipeRelatedService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;
    @Mock RecipeSimpleStaticDto recommendationDto;
    @Mock RecipeSimpleDto remixDto;

    @InjectMocks DevRecipeRelatedController controller;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;
    private static final Pageable PAGE = PageRequest.of(0, 10);

    // ---------- recommendations ----------

    @Test
    @DisplayName("recommendations anonymous: viewerId=null로 service 호출")
    void recommendations_anonymous_passesNullViewerId() {
        List<RecipeSimpleStaticDto> stub = List.of(recommendationDto);
        given(devRecipeRelatedService.getRecommendations(null, RECIPE_ID, 10)).willReturn(stub);

        ResponseEntity<List<RecipeSimpleStaticDto>> response = controller.getRecommendations(RECIPE_ID, 10, null);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(stub);
    }

    @Test
    @DisplayName("recommendations authenticated: viewerId=user.id로 service 호출 + size param 전달")
    void recommendations_authenticated_passesUserIdAndSize() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devRecipeRelatedService.getRecommendations(USER_ID, RECIPE_ID, 5)).willReturn(List.of());

        ResponseEntity<List<RecipeSimpleStaticDto>> response = controller.getRecommendations(RECIPE_ID, 5, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        verify(devRecipeRelatedService).getRecommendations(USER_ID, RECIPE_ID, 5);
    }

    @Test
    @DisplayName("recommendations: service throw → propagate")
    void recommendations_serviceThrows_propagates() {
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(devRecipeRelatedService).getRecommendations(null, RECIPE_ID, 10);

        assertThatThrownBy(() -> controller.getRecommendations(RECIPE_ID, 10, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);
    }

    // ---------- remixes ----------

    @Test
    @DisplayName("remixes anonymous: viewerId=null로 service 호출 + Page 반환")
    void remixes_anonymous_passesNullViewerId() {
        Page<RecipeSimpleDto> stub = new PageImpl<>(List.of(remixDto), PAGE, 1);
        given(devRecipeRelatedService.findRemixes(null, RECIPE_ID, PAGE)).willReturn(stub);

        ResponseEntity<Page<RecipeSimpleDto>> response = controller.getRemixes(RECIPE_ID, PAGE, null);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(stub);
    }

    @Test
    @DisplayName("remixes authenticated: viewerId=user.id로 service 호출")
    void remixes_authenticated_passesUserId() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        Page<RecipeSimpleDto> stub = new PageImpl<>(List.of(), PAGE, 0);
        given(devRecipeRelatedService.findRemixes(USER_ID, RECIPE_ID, PAGE)).willReturn(stub);

        ResponseEntity<Page<RecipeSimpleDto>> response = controller.getRemixes(RECIPE_ID, PAGE, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        verify(devRecipeRelatedService).findRemixes(USER_ID, RECIPE_ID, PAGE);
    }

    @Test
    @DisplayName("remixes: service throw → propagate")
    void remixes_serviceThrows_propagates() {
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(devRecipeRelatedService).findRemixes(null, RECIPE_ID, PAGE);

        assertThatThrownBy(() -> controller.getRemixes(RECIPE_ID, PAGE, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }
}
