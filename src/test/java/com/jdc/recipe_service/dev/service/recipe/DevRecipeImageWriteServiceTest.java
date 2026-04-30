package com.jdc.recipe_service.dev.service.recipe;

import com.jdc.recipe_service.domain.dto.recipe.RecipeImageKeyUpdateRequest;
import com.jdc.recipe_service.domain.dto.url.FileInfoRequest;
import com.jdc.recipe_service.domain.dto.url.FinalizeResponse;
import com.jdc.recipe_service.domain.dto.url.UpdatePresignedUrlResponse;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeLifecycleStatus;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeService;
import com.jdc.recipe_service.service.RecipeUploadService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRecipeImageWriteService 분기 매트릭스.
 *
 * 핵심 invariants:
 *  - 세 endpoint 공통: owner + ACTIVE 게이트가 운영 service 호출 전에 적용됨
 *  - presigned-urls: 운영의 ownership 미검사 leak을 dev pre-check가 차단
 *  - finalize: dev V3는 admin escape 없음 — isAdmin=false 강제 전달
 *  - updateImageKeys: payload null guard 추가
 */
@ExtendWith(MockitoExtension.class)
class DevRecipeImageWriteServiceTest {

    @Mock RecipeRepository recipeRepository;
    @Mock RecipeUploadService recipeUploadService;
    @Mock RecipeService recipeService;
    @Mock Recipe recipe;
    @Mock User owner;
    @Mock User otherUser;
    @Mock UpdatePresignedUrlResponse presignedResponse;
    @Mock FinalizeResponse finalizeResponse;

    @InjectMocks DevRecipeImageWriteService devRecipeImageWriteService;

    private static final Long USER_ID = 7L;
    private static final Long OTHER_ID = 99L;
    private static final Long RECIPE_ID = 100L;
    private static final List<FileInfoRequest> FILES = List.of();

    // ---------- presigned-urls ----------

    @Test
    @DisplayName("[presigned] owner + ACTIVE → 운영 RecipeUploadService 위임")
    void presigned_ownerActive_delegates() {
        givenOwnerActive();
        given(recipeUploadService.generatePresignedUrlsForUpdate(RECIPE_ID, USER_ID, FILES))
                .willReturn(presignedResponse);

        UpdatePresignedUrlResponse result = devRecipeImageWriteService
                .generatePresignedUrlsForUpdate(USER_ID, RECIPE_ID, FILES);

        assertThat(result).isSameAs(presignedResponse);
    }

    @Test
    @DisplayName("[presigned] **🚨 운영 leak 차단**: non-owner → RECIPE_ACCESS_DENIED, RecipeUploadService 미호출 (운영은 ownership 검사 자체 없어 누구나 호출 가능했음)")
    void presigned_nonOwner_throwsAccessDeniedAndSkipsUploadService() {
        givenRecipeOwnedBy(otherUser, OTHER_ID);

        assertThatThrownBy(() -> devRecipeImageWriteService
                .generatePresignedUrlsForUpdate(USER_ID, RECIPE_ID, FILES))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);

        verifyNoInteractions(recipeUploadService);
    }

    @Test
    @DisplayName("[presigned] owner + non-ACTIVE → RECIPE_NOT_FOUND, 운영 service 미호출")
    void presigned_nonActive_throwsNotFound() {
        givenOwnerWithLifecycle(RecipeLifecycleStatus.HIDDEN);

        assertThatThrownBy(() -> devRecipeImageWriteService
                .generatePresignedUrlsForUpdate(USER_ID, RECIPE_ID, FILES))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        verifyNoInteractions(recipeUploadService);
    }

    @Test
    @DisplayName("[presigned] 레시피 없음 → RECIPE_NOT_FOUND")
    void presigned_recipeNotFound_throws() {
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.empty());

        assertThatThrownBy(() -> devRecipeImageWriteService
                .generatePresignedUrlsForUpdate(USER_ID, RECIPE_ID, FILES))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    @Test
    @DisplayName("[presigned] **SHOULD 회귀 차단**: files=null → INVALID_INPUT_VALUE pre-DB (운영 stream() NPE 회피, recipeRepository 미호출)")
    void presigned_nullFiles_throwsInvalidInputBeforeDbHit() {
        assertThatThrownBy(() -> devRecipeImageWriteService
                .generatePresignedUrlsForUpdate(USER_ID, RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(recipeRepository, never()).findById(any());
        verifyNoInteractions(recipeUploadService);
    }

    @Test
    @DisplayName("[presigned] 빈 files 리스트는 허용 → 운영 service에 위임 (운영이 빈 uploads 응답 반환)")
    void presigned_emptyFiles_delegates() {
        givenOwnerActive();
        given(recipeUploadService.generatePresignedUrlsForUpdate(RECIPE_ID, USER_ID, List.of()))
                .willReturn(presignedResponse);

        UpdatePresignedUrlResponse result = devRecipeImageWriteService
                .generatePresignedUrlsForUpdate(USER_ID, RECIPE_ID, List.of());

        assertThat(result).isSameAs(presignedResponse);
    }

    // ---------- finalize ----------

    @Test
    @DisplayName("[finalize] owner + ACTIVE → 운영 finalize에 isAdmin=false 강제 전달")
    void finalize_ownerActive_delegatesWithIsAdminFalse() {
        givenOwnerActive();
        given(recipeService.finalizeRecipeImages(RECIPE_ID, USER_ID, false)).willReturn(finalizeResponse);

        FinalizeResponse result = devRecipeImageWriteService.finalizeRecipeImages(USER_ID, RECIPE_ID);

        assertThat(result).isSameAs(finalizeResponse);
        // 핵심 invariant: dev V3는 admin escape 미적용
        verify(recipeService).finalizeRecipeImages(RECIPE_ID, USER_ID, false);
    }

    @Test
    @DisplayName("[finalize] non-owner → RECIPE_ACCESS_DENIED, 운영 service 미호출")
    void finalize_nonOwner_throwsAccessDenied() {
        givenRecipeOwnedBy(otherUser, OTHER_ID);

        assertThatThrownBy(() -> devRecipeImageWriteService.finalizeRecipeImages(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);

        verifyNoInteractions(recipeService);
    }

    @Test
    @DisplayName("[finalize] owner + BANNED → RECIPE_NOT_FOUND")
    void finalize_ownerBanned_throwsNotFound() {
        givenOwnerWithLifecycle(RecipeLifecycleStatus.BANNED);

        assertThatThrownBy(() -> devRecipeImageWriteService.finalizeRecipeImages(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        verifyNoInteractions(recipeService);
    }

    // ---------- updateImageKeys ----------

    @Test
    @DisplayName("[updateImageKeys] request=null → INVALID_INPUT_VALUE, recipeRepository 미호출 (불필요한 DB hit 회피)")
    void updateImageKeys_nullRequest_throwsBeforeDbHit() {
        assertThatThrownBy(() -> devRecipeImageWriteService.updateImageKeys(USER_ID, RECIPE_ID, null))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.INVALID_INPUT_VALUE);

        verify(recipeRepository, never()).findById(any());
        verifyNoInteractions(recipeService);
    }

    @Test
    @DisplayName("[updateImageKeys] owner + ACTIVE + stepImageKeys 있음 → 원본 그대로 운영 updateImageKeys 위임")
    void updateImageKeys_ownerActiveWithStepKeys_delegatesOriginal() {
        RecipeImageKeyUpdateRequest req = new RecipeImageKeyUpdateRequest("main.webp", List.of("step1.webp"));
        givenOwnerActive();

        devRecipeImageWriteService.updateImageKeys(USER_ID, RECIPE_ID, req);

        // stepImageKeys가 non-null이면 normalize 안 함 → 원본 instance 그대로 전달
        verify(recipeService).updateImageKeys(RECIPE_ID, USER_ID, req);
    }

    @Test
    @DisplayName("[updateImageKeys] **SHOULD 회귀 차단**: stepImageKeys=null → 빈 리스트로 normalize, 운영 service에 normalized request 전달 (main-image-only update 지원)")
    void updateImageKeys_nullStepImageKeys_normalizesToEmptyList() {
        RecipeImageKeyUpdateRequest req = new RecipeImageKeyUpdateRequest("main.webp", null);
        givenOwnerActive();

        devRecipeImageWriteService.updateImageKeys(USER_ID, RECIPE_ID, req);

        // 운영 service에는 normalized request (stepImageKeys=[]) 전달 — IntStream.range NPE 회피
        ArgumentCaptor<RecipeImageKeyUpdateRequest> reqCap = ArgumentCaptor.forClass(RecipeImageKeyUpdateRequest.class);
        verify(recipeService).updateImageKeys(eq(RECIPE_ID), eq(USER_ID), reqCap.capture());
        RecipeImageKeyUpdateRequest passed = reqCap.getValue();
        assertThat(passed.getImageKey()).isEqualTo("main.webp");
        assertThat(passed.getStepImageKeys()).isEmpty();
        assertThat(passed.getStepImageKeys()).isNotNull();
    }

    @Test
    @DisplayName("[updateImageKeys] stepImageKeys=빈 리스트 → 그대로 위임 (이미 정상)")
    void updateImageKeys_emptyStepImageKeys_passesThrough() {
        RecipeImageKeyUpdateRequest req = new RecipeImageKeyUpdateRequest("main.webp", List.of());
        givenOwnerActive();

        devRecipeImageWriteService.updateImageKeys(USER_ID, RECIPE_ID, req);

        verify(recipeService).updateImageKeys(RECIPE_ID, USER_ID, req);
    }

    @Test
    @DisplayName("[updateImageKeys] non-owner → RECIPE_ACCESS_DENIED")
    void updateImageKeys_nonOwner_throwsAccessDenied() {
        RecipeImageKeyUpdateRequest req = new RecipeImageKeyUpdateRequest("main.webp", List.of());
        givenRecipeOwnedBy(otherUser, OTHER_ID);

        assertThatThrownBy(() -> devRecipeImageWriteService.updateImageKeys(USER_ID, RECIPE_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);

        verifyNoInteractions(recipeService);
    }

    @Test
    @DisplayName("[updateImageKeys] owner + non-ACTIVE → RECIPE_NOT_FOUND")
    void updateImageKeys_nonActive_throwsNotFound() {
        RecipeImageKeyUpdateRequest req = new RecipeImageKeyUpdateRequest("main.webp", List.of());
        givenOwnerWithLifecycle(RecipeLifecycleStatus.DELETED);

        assertThatThrownBy(() -> devRecipeImageWriteService.updateImageKeys(USER_ID, RECIPE_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);
    }

    @Test
    @DisplayName("[updateImageKeys] recipe.user=null → RECIPE_ACCESS_DENIED (방어적 체크)")
    void updateImageKeys_nullUser_throwsAccessDenied() {
        RecipeImageKeyUpdateRequest req = new RecipeImageKeyUpdateRequest("main.webp", List.of());
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipe.getUser()).willReturn(null);

        assertThatThrownBy(() -> devRecipeImageWriteService.updateImageKeys(USER_ID, RECIPE_ID, req))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_ACCESS_DENIED);
    }

    // ---------- helpers ----------

    private void givenOwnerActive() {
        givenOwnerWithLifecycle(RecipeLifecycleStatus.ACTIVE);
    }

    private void givenOwnerWithLifecycle(RecipeLifecycleStatus lifecycle) {
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipe.getUser()).willReturn(owner);
        given(owner.getId()).willReturn(USER_ID);
        if (lifecycle == RecipeLifecycleStatus.ACTIVE) {
            given(recipe.getLifecycleStatus()).willReturn(RecipeLifecycleStatus.ACTIVE);
        } else {
            given(recipe.getLifecycleStatus()).willReturn(lifecycle);
        }
    }

    private void givenRecipeOwnedBy(User user, Long ownerId) {
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(recipe.getUser()).willReturn(user);
        given(user.getId()).willReturn(ownerId);
    }
}
