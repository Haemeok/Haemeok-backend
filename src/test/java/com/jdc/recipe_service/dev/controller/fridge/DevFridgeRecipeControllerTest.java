package com.jdc.recipe_service.dev.controller.fridge;

import com.jdc.recipe_service.dev.service.fridge.DevFridgeRecipeService;
import com.jdc.recipe_service.domain.dto.recipe.FridgeRecipeDto;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.RecipeType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.security.CustomUserDetails;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.SliceImpl;
import org.springframework.http.ResponseEntity;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevFridgeRecipeController 단위 테스트.
 *
 * 검증:
 *  1. userDetails=null → CustomException(UNAUTHORIZED) 즉시 throw, service 호출 안 함
 *  2. types=null → service에 [USER, YOUTUBE] 기본값 전달
 *  3. types=empty list → service에 [USER, YOUTUBE] 기본값 전달
 *  4. types=[USER, AI] → service에 그대로 전달 (default 무시)
 *  5. service 결과를 그대로 200 body로 반환
 */
@ExtendWith(MockitoExtension.class)
class DevFridgeRecipeControllerTest {

    @Mock DevFridgeRecipeService devFridgeRecipeService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevFridgeRecipeController controller;

    private static final Long USER_ID = 7L;
    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @Test
    @DisplayName("userDetails=null: UNAUTHORIZED 즉시 throw, service 호출 안 함")
    void anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.findByFridge(null, PAGE_10, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devFridgeRecipeService);
    }

    @Test
    @DisplayName("types=null: service에 [USER, YOUTUBE] 기본값 전달")
    void typesNull_defaultsToUserAndYoutube() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devFridgeRecipeService.searchByFridgeDev(eq(USER_ID), eq(PAGE_10), any()))
                .willReturn(new SliceImpl<>(List.of(), PAGE_10, false));

        controller.findByFridge(userDetails, PAGE_10, null);

        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<RecipeType>> typesCaptor = ArgumentCaptor.forClass(List.class);
        verify(devFridgeRecipeService).searchByFridgeDev(eq(USER_ID), eq(PAGE_10), typesCaptor.capture());
        assertThat(typesCaptor.getValue()).containsExactly(RecipeType.USER, RecipeType.YOUTUBE);
    }

    @Test
    @DisplayName("types=empty list: service에 [USER, YOUTUBE] 기본값 전달 (null과 동등 처리)")
    void typesEmpty_defaultsToUserAndYoutube() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devFridgeRecipeService.searchByFridgeDev(eq(USER_ID), eq(PAGE_10), any()))
                .willReturn(new SliceImpl<>(List.of(), PAGE_10, false));

        controller.findByFridge(userDetails, PAGE_10, List.of());

        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<RecipeType>> typesCaptor = ArgumentCaptor.forClass(List.class);
        verify(devFridgeRecipeService).searchByFridgeDev(eq(USER_ID), eq(PAGE_10), typesCaptor.capture());
        assertThat(typesCaptor.getValue()).containsExactly(RecipeType.USER, RecipeType.YOUTUBE);
    }

    @Test
    @DisplayName("types=[USER, AI]: service에 그대로 전달 (default 무시)")
    void typesExplicit_passedThrough() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        given(devFridgeRecipeService.searchByFridgeDev(eq(USER_ID), eq(PAGE_10), any()))
                .willReturn(new SliceImpl<>(List.of(), PAGE_10, false));

        controller.findByFridge(userDetails, PAGE_10, List.of(RecipeType.USER, RecipeType.AI));

        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<RecipeType>> typesCaptor = ArgumentCaptor.forClass(List.class);
        verify(devFridgeRecipeService).searchByFridgeDev(eq(USER_ID), eq(PAGE_10), typesCaptor.capture());
        assertThat(typesCaptor.getValue()).containsExactly(RecipeType.USER, RecipeType.AI);
    }

    @Test
    @DisplayName("정상 흐름: service 결과를 200 body로 그대로 반환")
    void happyPath_returnsServiceResultAs200() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        Slice<FridgeRecipeDto> serviceResult = new SliceImpl<>(List.of(), PAGE_10, false);
        given(devFridgeRecipeService.searchByFridgeDev(eq(USER_ID), eq(PAGE_10), any()))
                .willReturn(serviceResult);

        ResponseEntity<Slice<FridgeRecipeDto>> response = controller.findByFridge(userDetails, PAGE_10, null);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(serviceResult);
    }
}
