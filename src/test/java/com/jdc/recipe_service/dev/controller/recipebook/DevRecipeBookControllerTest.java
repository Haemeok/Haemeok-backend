package com.jdc.recipe_service.dev.controller.recipebook;

import com.jdc.recipe_service.dev.domain.dto.recipebook.DevRecipeBookDetailResponse;
import com.jdc.recipe_service.dev.service.recipebook.DevRecipeBookService;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeBookResponse;
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
 * DevRecipeBookController 단위 검증.
 *
 *  1. listBooks anonymous → UNAUTHORIZED + service 미호출
 *  2. listBooks authenticated → service에 user.id 전달
 *  3. getBookDetail anonymous → UNAUTHORIZED + service 미호출
 *  4. getBookDetail authenticated → service에 (user.id, bookId, pageable) 전달
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeBookControllerTest {

    @Mock DevRecipeBookService devRecipeBookService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;

    @InjectMocks DevRecipeBookController controller;

    private static final Long USER_ID = 7L;
    private static final Long BOOK_ID = 100L;
    private static final Pageable PAGE_10 = PageRequest.of(0, 10);

    @Test
    @DisplayName("listBooks anonymous: UNAUTHORIZED + service 미호출")
    void listBooks_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.listBooks(null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRecipeBookService);
    }

    @Test
    @DisplayName("listBooks authenticated: service에 user.id 전달 + 200")
    void listBooks_authenticated_passesUserId() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        List<RecipeBookResponse> serviceResult = List.of();
        given(devRecipeBookService.listBooksDev(USER_ID)).willReturn(serviceResult);

        ResponseEntity<List<RecipeBookResponse>> response = controller.listBooks(userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(serviceResult);
        verify(devRecipeBookService).listBooksDev(USER_ID);
    }

    @Test
    @DisplayName("getBookDetail anonymous: UNAUTHORIZED + service 미호출")
    void getBookDetail_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.getBookDetail(null, BOOK_ID, PAGE_10))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRecipeBookService);
    }

    @Test
    @DisplayName("getBookDetail authenticated: service에 (user.id, bookId, pageable) 전달")
    void getBookDetail_authenticated_passesArgs() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
        DevRecipeBookDetailResponse serviceResult = DevRecipeBookDetailResponse.builder()
                .id(BOOK_ID).name("my-book").recipeCount(0).build();
        given(devRecipeBookService.getBookDetailDev(eq(USER_ID), eq(BOOK_ID), eq(PAGE_10)))
                .willReturn(serviceResult);

        ResponseEntity<DevRecipeBookDetailResponse> response = controller.getBookDetail(userDetails, BOOK_ID, PAGE_10);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(serviceResult);
        verify(devRecipeBookService).getBookDetailDev(USER_ID, BOOK_ID, PAGE_10);
    }
}
