package com.jdc.recipe_service.dev.controller.recipebook;

import com.jdc.recipe_service.dev.service.recipebook.DevRecipeBookWriteService;
import com.jdc.recipe_service.domain.dto.recipebook.AddRecipesToBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.AddRecipesToBookResponse;
import com.jdc.recipe_service.domain.dto.recipebook.CreateRecipeBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeBookResponse;
import com.jdc.recipe_service.domain.dto.recipebook.RemoveRecipesFromBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.RenameRecipeBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.ReorderRecipeBooksRequest;
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
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeBookWriteController 단위 검증.
 *
 * 컨트롤러 책임: 인증 경계, service 위임, 응답 status (201/200). 분기 매트릭스는 service test가 잠근다.
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeBookWriteControllerTest {

    @Mock DevRecipeBookWriteService devRecipeBookWriteService;
    @Mock CustomUserDetails userDetails;
    @Mock User user;
    @Mock RecipeBookResponse bookResponse;

    @InjectMocks DevRecipeBookWriteController controller;

    private static final Long USER_ID = 7L;
    private static final Long BOOK_ID = 50L;

    // ---------- create ----------

    @Test
    @DisplayName("create anonymous: UNAUTHORIZED + service 미호출")
    void create_anonymous_throwsUnauthorized() {
        CreateRecipeBookRequest req = CreateRecipeBookRequest.builder().name("한식").build();

        assertThatThrownBy(() -> controller.createBook(req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);

        verifyNoInteractions(devRecipeBookWriteService);
    }

    @Test
    @DisplayName("create authenticated: 201 Created + body")
    void create_authenticated_returns201() {
        CreateRecipeBookRequest req = CreateRecipeBookRequest.builder().name("한식").build();
        givenAuth();
        given(devRecipeBookWriteService.createBook(USER_ID, req)).willReturn(bookResponse);

        ResponseEntity<RecipeBookResponse> response = controller.createBook(req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(201);
        assertThat(response.getBody()).isSameAs(bookResponse);
    }

    // ---------- rename ----------

    @Test
    @DisplayName("rename anonymous: UNAUTHORIZED")
    void rename_anonymous_throwsUnauthorized() {
        RenameRecipeBookRequest req = RenameRecipeBookRequest.builder().name("새이름").build();
        assertThatThrownBy(() -> controller.renameBook(BOOK_ID, req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devRecipeBookWriteService);
    }

    @Test
    @DisplayName("rename authenticated: 200 + service 위임")
    void rename_authenticated_returns200() {
        RenameRecipeBookRequest req = RenameRecipeBookRequest.builder().name("새이름").build();
        givenAuth();
        given(devRecipeBookWriteService.renameBook(USER_ID, BOOK_ID, req)).willReturn(bookResponse);

        ResponseEntity<RecipeBookResponse> response = controller.renameBook(BOOK_ID, req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(bookResponse);
    }

    // ---------- delete ----------

    @Test
    @DisplayName("delete anonymous: UNAUTHORIZED")
    void delete_anonymous_throwsUnauthorized() {
        assertThatThrownBy(() -> controller.deleteBook(BOOK_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devRecipeBookWriteService);
    }

    @Test
    @DisplayName("delete authenticated: 200 + 메시지 응답 + service 위임")
    void delete_authenticated_returns200WithMessage() {
        givenAuth();

        ResponseEntity<Map<String, String>> response = controller.deleteBook(BOOK_ID, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).containsEntry("message", "레시피북이 삭제되었습니다.");
        verify(devRecipeBookWriteService).deleteBook(USER_ID, BOOK_ID);
    }

    // ---------- reorder ----------

    @Test
    @DisplayName("reorder anonymous: UNAUTHORIZED")
    void reorder_anonymous_throwsUnauthorized() {
        ReorderRecipeBooksRequest req = ReorderRecipeBooksRequest.builder().bookIds(List.of()).build();
        assertThatThrownBy(() -> controller.reorderBooks(req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devRecipeBookWriteService);
    }

    @Test
    @DisplayName("reorder authenticated: 200 + service 위임")
    void reorder_authenticated_returns200() {
        ReorderRecipeBooksRequest req = ReorderRecipeBooksRequest.builder().bookIds(List.of(1L, 2L)).build();
        givenAuth();
        List<RecipeBookResponse> stub = List.of(bookResponse);
        given(devRecipeBookWriteService.reorderBooks(USER_ID, req)).willReturn(stub);

        ResponseEntity<List<RecipeBookResponse>> response = controller.reorderBooks(req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(stub);
    }

    // ---------- addRecipes ----------

    @Test
    @DisplayName("addRecipes anonymous: UNAUTHORIZED")
    void addRecipes_anonymous_throwsUnauthorized() {
        AddRecipesToBookRequest req = AddRecipesToBookRequest.builder().recipeIds(List.of(1L)).build();
        assertThatThrownBy(() -> controller.addRecipes(BOOK_ID, req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devRecipeBookWriteService);
    }

    @Test
    @DisplayName("addRecipes authenticated: 200 + service 위임")
    void addRecipes_authenticated_returns200() {
        AddRecipesToBookRequest req = AddRecipesToBookRequest.builder().recipeIds(List.of(1L, 2L)).build();
        givenAuth();
        AddRecipesToBookResponse stub = AddRecipesToBookResponse.builder().addedCount(2).skippedCount(0).build();
        given(devRecipeBookWriteService.addRecipesToBook(USER_ID, BOOK_ID, req)).willReturn(stub);

        ResponseEntity<AddRecipesToBookResponse> response = controller.addRecipes(BOOK_ID, req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).isSameAs(stub);
    }

    // ---------- removeRecipes ----------

    @Test
    @DisplayName("removeRecipes anonymous: UNAUTHORIZED")
    void removeRecipes_anonymous_throwsUnauthorized() {
        RemoveRecipesFromBookRequest req = RemoveRecipesFromBookRequest.builder().recipeIds(List.of(1L)).build();
        assertThatThrownBy(() -> controller.removeRecipes(BOOK_ID, req, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.UNAUTHORIZED);
        verifyNoInteractions(devRecipeBookWriteService);
    }

    @Test
    @DisplayName("removeRecipes authenticated: 200 + 메시지 응답 + service 위임")
    void removeRecipes_authenticated_returns200WithMessage() {
        RemoveRecipesFromBookRequest req = RemoveRecipesFromBookRequest.builder().recipeIds(List.of(1L, 2L)).build();
        givenAuth();

        ResponseEntity<Map<String, String>> response = controller.removeRecipes(BOOK_ID, req, userDetails);

        assertThat(response.getStatusCode().value()).isEqualTo(200);
        assertThat(response.getBody()).containsEntry("message", "레시피가 레시피북에서 삭제되었습니다.");
        verify(devRecipeBookWriteService).removeRecipesFromBook(USER_ID, BOOK_ID, req);
    }

    // ---------- helpers ----------

    private void givenAuth() {
        given(userDetails.getUser()).willReturn(user);
        given(user.getId()).willReturn(USER_ID);
    }
}
