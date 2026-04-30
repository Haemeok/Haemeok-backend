package com.jdc.recipe_service.dev.controller.favorite;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevRecipeSimpleDto;
import com.jdc.recipe_service.dev.service.favorite.DevFavoritesService;
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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevFavoritesController 단위 검증.
 *
 *  1. anonymous → UNAUTHORIZED + service 호출 없음
 *  2. authenticated → service에 user.id 전달 + 200 body
 */
@ExtendWith(MockitoExtension.class)
class DevFavoritesControllerTest {

    @Mock DevFavoritesService devFavoritesService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevFavoritesController controller;

    private static final Long USER_ID = 7L;
    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @Test
    @DisplayName("anonymous: UNAUTHORIZED + service 호출 없음")
    void anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.getMyFavorites(null, PAGE_10))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devFavoritesService);
    }

    @Test
    @DisplayName("authenticated: service에 user.id 전달 + 200 body 반환")
    void authenticated_passesUserIdAndReturns200() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        Page<DevRecipeSimpleDto> servicePage = new PageImpl<>(List.of(), PAGE_10, 0);
        given(devFavoritesService.getMyFavoritesDev(eq(USER_ID), eq(PAGE_10))).willReturn(servicePage);

        ResponseEntity<Page<DevRecipeSimpleDto>> response = controller.getMyFavorites(userDetails, PAGE_10);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(servicePage);
        verify(devFavoritesService).getMyFavoritesDev(USER_ID, PAGE_10);
    }
}
