package com.jdc.recipe_service.dev.service.recipebook;

import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjection;
import com.jdc.recipe_service.dev.repository.recipe.DevRecipeAccessProjectionRepository;
import com.jdc.recipe_service.domain.dto.recipebook.AddRecipesToBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.AddRecipesToBookResponse;
import com.jdc.recipe_service.domain.dto.recipebook.CreateRecipeBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.RecipeBookResponse;
import com.jdc.recipe_service.domain.dto.recipebook.RemoveRecipesFromBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.RenameRecipeBookRequest;
import com.jdc.recipe_service.domain.dto.recipebook.ReorderRecipeBooksRequest;
import com.jdc.recipe_service.domain.type.RecipeImageStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeListingStatus;
import com.jdc.recipe_service.domain.type.recipe.RecipeVisibility;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeBookService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeBookWriteService 분기 매트릭스.
 *
 * 핵심 invariants:
 *  - 5 endpoint (create/rename/delete/reorder/remove): 단순 위임 + payload null guard
 *  - <b>addRecipesToBook</b>: 운영 isAccessible(isPrivate-only) leak을 dev 4-enum accessibleBy로 차단
 *    - skippedCount = 운영 skipped + dev filtered (RESTRICTED 등)
 *    - 모든 ID 차단 시에도 운영 호출 (book ownership check 강제)
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeBookWriteServiceTest {

    @Mock RecipeBookService recipeBookService;
    @Mock DevRecipeAccessProjectionRepository accessProjectionRepository;
    @Mock RecipeBookResponse bookResponse;

    @InjectMocks DevRecipeBookWriteService devRecipeBookWriteService;

    private static final Long USER_ID = 7L;
    private static final Long OWNER_ID = 99L;
    private static final Long BOOK_ID = 50L;

    // ---------- create ----------

    @Test
    @DisplayName("[create] payload null → INVALID_INPUT_VALUE, 운영 service 미호출")
    void create_nullRequest_throws() {
        assertThatThrownBy(() -> devRecipeBookWriteService.createBook(USER_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(recipeBookService);
    }

    @Test
    @DisplayName("[create] valid payload → 운영 service 위임")
    void create_validPayload_delegates() {
        CreateRecipeBookRequest req = CreateRecipeBookRequest.builder().name("한식").build();
        given(recipeBookService.createBook(USER_ID, req)).willReturn(bookResponse);

        RecipeBookResponse result = devRecipeBookWriteService.createBook(USER_ID, req);

        assertThat(result).isSameAs(bookResponse);
    }

    // ---------- rename ----------

    @Test
    @DisplayName("[rename] payload null → INVALID_INPUT_VALUE, 운영 service 미호출")
    void rename_nullRequest_throws() {
        assertThatThrownBy(() -> devRecipeBookWriteService.renameBook(USER_ID, BOOK_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(recipeBookService);
    }

    @Test
    @DisplayName("[rename] valid payload → 운영 service 위임")
    void rename_validPayload_delegates() {
        RenameRecipeBookRequest req = RenameRecipeBookRequest.builder().name("새이름").build();
        given(recipeBookService.renameBook(USER_ID, BOOK_ID, req)).willReturn(bookResponse);

        RecipeBookResponse result = devRecipeBookWriteService.renameBook(USER_ID, BOOK_ID, req);

        assertThat(result).isSameAs(bookResponse);
    }

    // ---------- delete ----------

    @Test
    @DisplayName("[delete] 단순 위임 (운영이 ownership/default check)")
    void delete_delegates() {
        devRecipeBookWriteService.deleteBook(USER_ID, BOOK_ID);
        verify(recipeBookService).deleteBook(USER_ID, BOOK_ID);
    }

    @Test
    @DisplayName("[delete] 운영 service throw → propagate")
    void delete_propagates() {
        willThrow(new CustomException(ErrorCode.RECIPE_BOOK_DEFAULT_CANNOT_DELETE))
                .given(recipeBookService).deleteBook(USER_ID, BOOK_ID);

        assertThatThrownBy(() -> devRecipeBookWriteService.deleteBook(USER_ID, BOOK_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_BOOK_DEFAULT_CANNOT_DELETE);
    }

    // ---------- reorder ----------

    @Test
    @DisplayName("[reorder] payload null → INVALID_INPUT_VALUE, 운영 service 미호출")
    void reorder_nullRequest_throws() {
        assertThatThrownBy(() -> devRecipeBookWriteService.reorderBooks(USER_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(recipeBookService);
    }

    @Test
    @DisplayName("[reorder] valid payload → 운영 service 위임")
    void reorder_validPayload_delegates() {
        ReorderRecipeBooksRequest req = ReorderRecipeBooksRequest.builder().bookIds(List.of(1L, 2L)).build();
        List<RecipeBookResponse> stub = List.of(bookResponse, bookResponse);
        given(recipeBookService.reorderBooks(USER_ID, req)).willReturn(stub);

        List<RecipeBookResponse> result = devRecipeBookWriteService.reorderBooks(USER_ID, req);

        assertThat(result).isSameAs(stub);
    }

    // ---------- addRecipesToBook (핵심) ----------

    @Test
    @DisplayName("[addRecipes] payload null → INVALID_INPUT_VALUE, 운영 service 미호출")
    void addRecipes_nullRequest_throws() {
        assertThatThrownBy(() -> devRecipeBookWriteService.addRecipesToBook(USER_ID, BOOK_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(recipeBookService, accessProjectionRepository);
    }

    @Test
    @DisplayName("[addRecipes] recipeIds=null → INVALID_INPUT_VALUE, 운영 service 미호출")
    void addRecipes_nullRecipeIds_throws() {
        AddRecipesToBookRequest req = AddRecipesToBookRequest.builder().recipeIds(null).build();
        assertThatThrownBy(() -> devRecipeBookWriteService.addRecipesToBook(USER_ID, BOOK_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(recipeBookService, accessProjectionRepository);
    }

    @Test
    @DisplayName("[addRecipes] **MUST 회귀 차단**: recipeIds=[] → 운영 service에 빈 list로 위임 (book ownership check 강제, service-layer 우회 불가)")
    void addRecipes_emptyRecipeIds_delegatesToOperationalForOwnershipCheck() {
        AddRecipesToBookRequest req = AddRecipesToBookRequest.builder().recipeIds(List.of()).build();
        given(recipeBookService.addRecipesToBook(eq(USER_ID), eq(BOOK_ID), any(AddRecipesToBookRequest.class)))
                .willReturn(AddRecipesToBookResponse.builder().addedCount(0).skippedCount(0).build());

        AddRecipesToBookResponse result = devRecipeBookWriteService.addRecipesToBook(USER_ID, BOOK_ID, req);

        assertThat(result.getAddedCount()).isZero();
        assertThat(result.getSkippedCount()).isZero();

        // 운영 호출 발생 (ownership check 강제) — 빈 리스트로
        ArgumentCaptor<AddRecipesToBookRequest> reqCap = ArgumentCaptor.forClass(AddRecipesToBookRequest.class);
        verify(recipeBookService).addRecipesToBook(eq(USER_ID), eq(BOOK_ID), reqCap.capture());
        assertThat(reqCap.getValue().getRecipeIds()).isEmpty();
    }

    @Test
    @DisplayName("[addRecipes] recipeIds=[] + 운영이 RECIPE_BOOK_NOT_FOUND throw → 그대로 propagate (ownership check 우선)")
    void addRecipes_emptyRecipeIdsBookNotFound_throwsPropagate() {
        AddRecipesToBookRequest req = AddRecipesToBookRequest.builder().recipeIds(List.of()).build();
        willThrow(new CustomException(ErrorCode.RECIPE_BOOK_NOT_FOUND))
                .given(recipeBookService).addRecipesToBook(eq(USER_ID), eq(BOOK_ID), any(AddRecipesToBookRequest.class));

        assertThatThrownBy(() -> devRecipeBookWriteService.addRecipesToBook(USER_ID, BOOK_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_BOOK_NOT_FOUND);
    }

    @Test
    @DisplayName("[addRecipes] 모두 accessible → 전부 운영 service에 전달, dev filtered=0")
    void addRecipes_allAccessible_passesAllToOperational() {
        AddRecipesToBookRequest req = AddRecipesToBookRequest.builder().recipeIds(List.of(1L, 2L, 3L)).build();
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L, 3L)))
                .willReturn(List.of(publicListed(1L), publicListed(2L), publicListed(3L)));
        given(recipeBookService.addRecipesToBook(eq(USER_ID), eq(BOOK_ID), any(AddRecipesToBookRequest.class)))
                .willReturn(AddRecipesToBookResponse.builder().addedCount(3).skippedCount(0).build());

        AddRecipesToBookResponse result = devRecipeBookWriteService.addRecipesToBook(USER_ID, BOOK_ID, req);

        assertThat(result.getAddedCount()).isEqualTo(3);
        assertThat(result.getSkippedCount()).isZero();

        // 운영 service에 3개 모두 전달되었는지 확인
        ArgumentCaptor<AddRecipesToBookRequest> reqCap = ArgumentCaptor.forClass(AddRecipesToBookRequest.class);
        verify(recipeBookService).addRecipesToBook(eq(USER_ID), eq(BOOK_ID), reqCap.capture());
        assertThat(reqCap.getValue().getRecipeIds()).containsExactly(1L, 2L, 3L);
    }

    @Test
    @DisplayName("[addRecipes] **🚨 운영 leak 차단**: RESTRICTED 섞임 → dev에서 사전 필터, 운영은 accessible-only로 호출, skippedCount 합산")
    void addRecipes_restrictedFilteredOut_combinesSkipCounts() {
        AddRecipesToBookRequest req = AddRecipesToBookRequest.builder().recipeIds(List.of(1L, 2L, 3L)).build();
        // 1=PUBLIC, 2=RESTRICTED (dev filter), 3=PUBLIC
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L, 3L)))
                .willReturn(List.of(publicListed(1L), restricted(2L), publicListed(3L)));
        // 운영: 1, 3만 전달받고 그 중 1은 이미 폴더에 있음 → addedCount=1, opSkipped=1
        given(recipeBookService.addRecipesToBook(eq(USER_ID), eq(BOOK_ID), any(AddRecipesToBookRequest.class)))
                .willReturn(AddRecipesToBookResponse.builder().addedCount(1).skippedCount(1).build());

        AddRecipesToBookResponse result = devRecipeBookWriteService.addRecipesToBook(USER_ID, BOOK_ID, req);

        assertThat(result.getAddedCount()).isEqualTo(1);
        // 합산: 운영 skipped(1, 이미 있음) + dev filtered(1, RESTRICTED) = 2
        assertThat(result.getSkippedCount()).isEqualTo(2);

        ArgumentCaptor<AddRecipesToBookRequest> reqCap = ArgumentCaptor.forClass(AddRecipesToBookRequest.class);
        verify(recipeBookService).addRecipesToBook(eq(USER_ID), eq(BOOK_ID), reqCap.capture());
        assertThat(reqCap.getValue().getRecipeIds()).containsExactly(1L, 3L); // RESTRICTED(2L) 제외
    }

    @Test
    @DisplayName("[addRecipes] 모든 ID 차단됨 → 운영 service에 빈 리스트로 호출 (book ownership check 강제), skippedCount=devFiltered")
    void addRecipes_allBlocked_stillCallsOperationalForOwnershipCheck() {
        AddRecipesToBookRequest req = AddRecipesToBookRequest.builder().recipeIds(List.of(1L, 2L)).build();
        // 모두 RESTRICTED
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L)))
                .willReturn(List.of(restricted(1L), restricted(2L)));
        given(recipeBookService.addRecipesToBook(eq(USER_ID), eq(BOOK_ID), any(AddRecipesToBookRequest.class)))
                .willReturn(AddRecipesToBookResponse.builder().addedCount(0).skippedCount(0).build());

        AddRecipesToBookResponse result = devRecipeBookWriteService.addRecipesToBook(USER_ID, BOOK_ID, req);

        assertThat(result.getAddedCount()).isZero();
        assertThat(result.getSkippedCount()).isEqualTo(2);

        // 운영 호출 발생 (book ownership check 강제) — 빈 리스트로
        ArgumentCaptor<AddRecipesToBookRequest> reqCap = ArgumentCaptor.forClass(AddRecipesToBookRequest.class);
        verify(recipeBookService).addRecipesToBook(eq(USER_ID), eq(BOOK_ID), reqCap.capture());
        assertThat(reqCap.getValue().getRecipeIds()).isEmpty();
    }

    @Test
    @DisplayName("[addRecipes] 모든 ID 차단됨 + 운영이 RECIPE_BOOK_NOT_FOUND throw → 그대로 propagate (ownership check 우선)")
    void addRecipes_allBlockedAndBookNotFound_throwsPropagate() {
        AddRecipesToBookRequest req = AddRecipesToBookRequest.builder().recipeIds(List.of(1L)).build();
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L)))
                .willReturn(List.of(restricted(1L)));
        willThrow(new CustomException(ErrorCode.RECIPE_BOOK_NOT_FOUND))
                .given(recipeBookService).addRecipesToBook(eq(USER_ID), eq(BOOK_ID), any(AddRecipesToBookRequest.class));

        assertThatThrownBy(() -> devRecipeBookWriteService.addRecipesToBook(USER_ID, BOOK_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_BOOK_NOT_FOUND);
    }

    @Test
    @DisplayName("[addRecipes] 중복 ID는 distinct 처리 → projection 조회는 unique IDs만")
    void addRecipes_duplicateIds_distinctBeforeProjection() {
        AddRecipesToBookRequest req = AddRecipesToBookRequest.builder().recipeIds(List.of(1L, 1L, 2L, 2L)).build();
        given(accessProjectionRepository.findAccessProjectionsByIds(List.of(1L, 2L)))
                .willReturn(List.of(publicListed(1L), publicListed(2L)));
        given(recipeBookService.addRecipesToBook(eq(USER_ID), eq(BOOK_ID), any(AddRecipesToBookRequest.class)))
                .willReturn(AddRecipesToBookResponse.builder().addedCount(2).skippedCount(0).build());

        devRecipeBookWriteService.addRecipesToBook(USER_ID, BOOK_ID, req);

        verify(accessProjectionRepository, times(1)).findAccessProjectionsByIds(List.of(1L, 2L));
    }

    // ---------- removeRecipesFromBook ----------

    @Test
    @DisplayName("[removeRecipes] payload null → INVALID_INPUT_VALUE")
    void removeRecipes_nullRequest_throws() {
        assertThatThrownBy(() -> devRecipeBookWriteService.removeRecipesFromBook(USER_ID, BOOK_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);
        verifyNoInteractions(recipeBookService);
    }

    @Test
    @DisplayName("[removeRecipes] cleanup right — 게이트 없이 운영 service 위임 (RESTRICTED 레시피라도 본인 폴더에서 제거 가능)")
    void removeRecipes_noGateDelegates() {
        RemoveRecipesFromBookRequest req = RemoveRecipesFromBookRequest.builder().recipeIds(List.of(1L, 2L)).build();

        devRecipeBookWriteService.removeRecipesFromBook(USER_ID, BOOK_ID, req);

        verify(recipeBookService).removeRecipesFromBook(USER_ID, BOOK_ID, req);
        // accessProjection 호출 안 함 (cleanup right)
        verify(accessProjectionRepository, never()).findAccessProjectionsByIds(any());
    }

    // ---------- helpers ----------

    private static DevRecipeAccessProjection publicListed(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.PUBLIC, RecipeListingStatus.LISTED,
                RecipeImageStatus.READY, null);
    }

    private static DevRecipeAccessProjection restricted(Long id) {
        return new DevRecipeAccessProjection(id, OWNER_ID,
                RecipeLifecycleStatus.ACTIVE, RecipeVisibility.RESTRICTED, RecipeListingStatus.UNLISTED,
                RecipeImageStatus.READY, null);
    }
}
