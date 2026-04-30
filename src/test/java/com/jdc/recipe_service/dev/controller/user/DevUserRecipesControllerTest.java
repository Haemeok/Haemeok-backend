package com.jdc.recipe_service.dev.controller.user;

import com.jdc.recipe_service.dev.domain.dto.recipe.DevMyRecipeSummaryDto;
import com.jdc.recipe_service.dev.service.user.DevUserRecipesService;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevUserRecipesController 단위 검증.
 *
 *  1. /users/{id} anonymous: viewerId=null로 service에 전달
 *  2. /users/{id} authenticated: viewerId=user.id 전달
 *  3. /me/recipes anonymous: UNAUTHORIZED + service 호출 없음
 *  4. /me/recipes authenticated: viewerId == targetUserId (자동 owner 분기)
 *  5. types 파라미터 그대로 service에 전달
 */
@ExtendWith(MockitoExtension.class)
class DevUserRecipesControllerTest {

    @Mock DevUserRecipesService devUserRecipesService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevUserRecipesController controller;

    private static final Long TARGET_USER_ID = 100L;
    private static final Long VIEWER_ID = 7L;
    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    // ---------- /api/dev/users/{userId}/recipes ----------

    @Test
    @DisplayName("/users/{id} anonymous: service에 viewerId=null 전달 + 200")
    void getUserRecipes_anonymous_passesNullViewerId() {
        Page<DevMyRecipeSummaryDto> page = new PageImpl<>(List.of(), PAGE_10, 0);
        given(devUserRecipesService.getUserRecipesDev(eq(TARGET_USER_ID), isNull(), any(), eq(PAGE_10)))
                .willReturn(page);

        ResponseEntity<Page<DevMyRecipeSummaryDto>> response =
                controller.getUserRecipes(TARGET_USER_ID, null, null, PAGE_10);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        verify(devUserRecipesService).getUserRecipesDev(eq(TARGET_USER_ID), isNull(), any(), eq(PAGE_10));
    }

    @Test
    @DisplayName("/users/{id} authenticated: service에 viewerId=user.id 전달")
    void getUserRecipes_authenticated_passesUserId() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(VIEWER_ID);
        Page<DevMyRecipeSummaryDto> page = new PageImpl<>(List.of(), PAGE_10, 0);
        given(devUserRecipesService.getUserRecipesDev(eq(TARGET_USER_ID), eq(VIEWER_ID), any(), eq(PAGE_10)))
                .willReturn(page);

        controller.getUserRecipes(TARGET_USER_ID, userDetails, null, PAGE_10);

        verify(devUserRecipesService).getUserRecipesDev(eq(TARGET_USER_ID), eq(VIEWER_ID), any(), eq(PAGE_10));
    }

    @Test
    @DisplayName("/users/{id} types 파라미터 그대로 service에 전달")
    void getUserRecipes_typesPassedThrough() {
        List<RecipeSourceType> types = List.of(RecipeSourceType.USER, RecipeSourceType.YOUTUBE);
        Page<DevMyRecipeSummaryDto> page = new PageImpl<>(List.of(), PAGE_10, 0);
        given(devUserRecipesService.getUserRecipesDev(eq(TARGET_USER_ID), isNull(), eq(types), eq(PAGE_10)))
                .willReturn(page);

        controller.getUserRecipes(TARGET_USER_ID, null, types, PAGE_10);

        verify(devUserRecipesService).getUserRecipesDev(eq(TARGET_USER_ID), isNull(), eq(types), eq(PAGE_10));
    }

    // ---------- /api/dev/me/recipes ----------

    @Test
    @DisplayName("/me/recipes anonymous: UNAUTHORIZED + service 호출 없음")
    void getMyRecipes_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.getMyRecipes(null, null, PAGE_10))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devUserRecipesService);
    }

    @Test
    @DisplayName("/me/recipes authenticated: viewerId == targetUserId (자동 owner 분기 게이트)")
    void getMyRecipes_authenticated_viewerEqualsTarget() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(VIEWER_ID);
        Page<DevMyRecipeSummaryDto> page = new PageImpl<>(List.of(), PAGE_10, 0);
        given(devUserRecipesService.getUserRecipesDev(eq(VIEWER_ID), eq(VIEWER_ID), any(), eq(PAGE_10)))
                .willReturn(page);

        controller.getMyRecipes(userDetails, null, PAGE_10);

        // 핵심 invariant: viewer == target → service의 accessibleBy가 owner 분기로 자동 떨어져 PRIVATE/RESTRICTED 노출.
        // 회귀 시 다른 userId가 들어가면 owner 분기 깨짐.
        verify(devUserRecipesService).getUserRecipesDev(eq(VIEWER_ID), eq(VIEWER_ID), any(), eq(PAGE_10));
    }

    @Test
    @DisplayName("/me/recipes types 파라미터 전달")
    void getMyRecipes_typesPassedThrough() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(VIEWER_ID);
        List<RecipeSourceType> types = List.of(RecipeSourceType.AI);
        Page<DevMyRecipeSummaryDto> page = new PageImpl<>(List.of(), PAGE_10, 0);
        given(devUserRecipesService.getUserRecipesDev(eq(VIEWER_ID), eq(VIEWER_ID), eq(types), eq(PAGE_10)))
                .willReturn(page);

        controller.getMyRecipes(userDetails, types, PAGE_10);

        verify(devUserRecipesService).getUserRecipesDev(eq(VIEWER_ID), eq(VIEWER_ID), eq(types), eq(PAGE_10));
    }
}
