package com.jdc.recipe_service.dev.service.interaction;

import com.jdc.recipe_service.domain.entity.RecipeLike;
import com.jdc.recipe_service.domain.repository.RecipeBookItemRepository;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeBookService;
import com.jdc.recipe_service.service.RecipeLikeService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevInteractionService 분기 매트릭스.
 *
 * 핵심 invariant:
 *  - 게이트는 신규 추가 경로에만 적용. 기존 interaction 제거는 항상 허용.
 *  - like는 NotificationService REQUIRES_NEW 누수 방지를 위해 분기로 race add 차단:
 *     existing → remove-only 직접 처리 (운영 service 미호출)
 *     new → validator 통과 후에만 운영 service 호출
 *  - favorite은 REQUIRES_NEW side effect 없어 pre-check + post-check race recovery 패턴 유지.
 *
 * 회귀 방지:
 *  - 레시피 RESTRICTED 전환 후 본인 like/favorite 정리 불가 버그
 *  - race로 RESTRICTED 레시피에 게이트 우회 like + 알림 누수
 */
@ExtendWith(MockitoExtension.class)
class DevInteractionServiceTest {

    @Mock DevRecipeAccessValidator accessValidator;
    @Mock RecipeLikeRepository likeRepository;
    @Mock RecipeRepository recipeRepository;
    @Mock RecipeBookItemRepository bookItemRepository;
    @Mock RecipeLikeService likeService;
    @Mock RecipeBookService recipeBookService;
    @Mock RecipeLike existingLike;

    @InjectMocks DevInteractionService devInteractionService;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;

    // ---------- toggleLike (분기로 race add 차단) ----------

    @Test
    @DisplayName("[like] existing → remove-only 직접 처리: like 삭제 + decrementLikeCount, 운영 likeService 미호출 (알림 누수 차단)")
    void toggleLike_existing_removesDirectlyWithoutOperationalService() {
        given(likeRepository.existsByRecipeIdAndUserId(RECIPE_ID, USER_ID)).willReturn(true);
        given(likeRepository.findByUserIdAndRecipeId(USER_ID, RECIPE_ID)).willReturn(Optional.of(existingLike));

        boolean result = devInteractionService.toggleLike(USER_ID, RECIPE_ID);

        assertThat(result).isFalse();
        verify(likeRepository).delete(existingLike);
        verify(recipeRepository).decrementLikeCount(RECIPE_ID);
        verifyNoInteractions(accessValidator, likeService);
    }

    @Test
    @DisplayName("[like] existing이지만 race로 이미 사라짐 → no-op으로 false 반환, add 절대 안 함 (운영 likeService 미호출)")
    void toggleLike_existingButRaceAlreadyRemoved_noOpFalse() {
        // pre-check 시점엔 있었지만 findByUserIdAndRecipeId 시점엔 다른 요청이 이미 삭제
        given(likeRepository.existsByRecipeIdAndUserId(RECIPE_ID, USER_ID)).willReturn(true);
        given(likeRepository.findByUserIdAndRecipeId(USER_ID, RECIPE_ID)).willReturn(Optional.empty());

        boolean result = devInteractionService.toggleLike(USER_ID, RECIPE_ID);

        assertThat(result).isFalse();
        // 결정적 invariant: race가 일어나도 likeService.toggleLike(=add 가능 경로)는 절대 호출되지 않는다
        verifyNoInteractions(accessValidator, likeService);
        verify(recipeRepository, times(0)).decrementLikeCount(RECIPE_ID);
    }

    @Test
    @DisplayName("[like] new + validator 통과 → 운영 likeService에 위임하여 add (알림은 게이트 통과한 add에만)")
    void toggleLike_newWithValidatorPass_callsLikeService() {
        given(likeRepository.existsByRecipeIdAndUserId(RECIPE_ID, USER_ID)).willReturn(false);
        given(likeService.toggleLike(USER_ID, RECIPE_ID)).willReturn(true);

        boolean result = devInteractionService.toggleLike(USER_ID, RECIPE_ID);

        assertThat(result).isTrue();
        InOrder order = inOrder(accessValidator, likeService);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        order.verify(likeService).toggleLike(USER_ID, RECIPE_ID);
        // remove-only 경로 미진입 확인
        verify(likeRepository, times(0)).findByUserIdAndRecipeId(USER_ID, RECIPE_ID);
        verify(likeRepository, times(0)).delete(existingLike);
    }

    @Test
    @DisplayName("[like] new + validator throw → 운영 likeService 미호출 (RESTRICTED non-owner 신규 차단)")
    void toggleLike_newWithValidatorThrow_skipsLikeService() {
        given(likeRepository.existsByRecipeIdAndUserId(RECIPE_ID, USER_ID)).willReturn(false);
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devInteractionService.toggleLike(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(likeService);
    }

    // ---------- toggleFavorite (post-check race recovery — REQUIRES_NEW 없어 안전) ----------

    @Test
    @DisplayName("[favorite] 기존 저장 있음 + delegate가 false 반환 (정상 정리) → validator 우회 (정리 경로 보장)")
    void toggleFavorite_existing_bypassesValidatorAndDelegates() {
        given(bookItemRepository.existsByUserIdAndRecipeId(USER_ID, RECIPE_ID)).willReturn(true);
        given(recipeBookService.toggleSave(USER_ID, RECIPE_ID)).willReturn(false);

        boolean result = devInteractionService.toggleFavorite(USER_ID, RECIPE_ID);

        assertThat(result).isFalse();
        verify(recipeBookService).toggleSave(USER_ID, RECIPE_ID);
        verifyNoInteractions(accessValidator);
    }

    @Test
    @DisplayName("[favorite] race recovery: pre-check=existing이지만 delegate가 add(true) → post-check 호출 → throw 시 propagate (outer @Transactional rollback)")
    void toggleFavorite_raceAddedAfterPreCheck_postCheckThrows() {
        given(bookItemRepository.existsByUserIdAndRecipeId(USER_ID, RECIPE_ID)).willReturn(true);
        given(recipeBookService.toggleSave(USER_ID, RECIPE_ID)).willReturn(true);
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devInteractionService.toggleFavorite(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verify(accessValidator, times(1)).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        verify(recipeBookService).toggleSave(USER_ID, RECIPE_ID);
    }

    @Test
    @DisplayName("[favorite] race recovery: pre-check=existing + delegate가 add(true) + post-check 통과 (PUBLIC) → true 반환")
    void toggleFavorite_raceAddedButStillPublic_postCheckPasses() {
        given(bookItemRepository.existsByUserIdAndRecipeId(USER_ID, RECIPE_ID)).willReturn(true);
        given(recipeBookService.toggleSave(USER_ID, RECIPE_ID)).willReturn(true);

        boolean result = devInteractionService.toggleFavorite(USER_ID, RECIPE_ID);

        assertThat(result).isTrue();
        verify(accessValidator, times(1)).loadAndCheckInteractable(RECIPE_ID, USER_ID);
    }

    @Test
    @DisplayName("[favorite] 기존 저장 없음 + validator 통과 → recipeBookService 호출하여 true 반환 (신규 추가 경로)")
    void toggleFavorite_newWithValidatorPass_callsRecipeBookService() {
        given(bookItemRepository.existsByUserIdAndRecipeId(USER_ID, RECIPE_ID)).willReturn(false);
        given(recipeBookService.toggleSave(USER_ID, RECIPE_ID)).willReturn(true);

        boolean result = devInteractionService.toggleFavorite(USER_ID, RECIPE_ID);

        assertThat(result).isTrue();
        InOrder order = inOrder(accessValidator, recipeBookService);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        order.verify(recipeBookService).toggleSave(USER_ID, RECIPE_ID);
    }

    @Test
    @DisplayName("[favorite] 기존 저장 없음 + validator throw → recipeBookService 미호출 (신규 추가 차단)")
    void toggleFavorite_newWithValidatorThrow_skipsRecipeBookService() {
        given(bookItemRepository.existsByUserIdAndRecipeId(USER_ID, RECIPE_ID)).willReturn(false);
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devInteractionService.toggleFavorite(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        verifyNoInteractions(recipeBookService);
    }

    // ---------- 게이트 분기 source-of-truth (운영 정합) ----------

    @Test
    @DisplayName("[like] source of truth는 RecipeLikeRepository.existsByRecipeIdAndUserId — 다른 시그니처 미사용")
    void toggleLike_usesLikeRepositoryAsSourceOfTruth() {
        given(likeRepository.existsByRecipeIdAndUserId(RECIPE_ID, USER_ID)).willReturn(false);
        given(likeService.toggleLike(USER_ID, RECIPE_ID)).willReturn(true);

        devInteractionService.toggleLike(USER_ID, RECIPE_ID);

        verify(likeRepository).existsByRecipeIdAndUserId(RECIPE_ID, USER_ID);
        verifyNoInteractions(bookItemRepository);
    }

    @Test
    @DisplayName("[favorite] source of truth는 RecipeBookItemRepository — 운영 toggleSave 기준과 동일 (legacy recipe_favorites 아님)")
    void toggleFavorite_usesBookItemRepositoryAsSourceOfTruth() {
        given(bookItemRepository.existsByUserIdAndRecipeId(USER_ID, RECIPE_ID)).willReturn(false);
        given(recipeBookService.toggleSave(USER_ID, RECIPE_ID)).willReturn(true);

        devInteractionService.toggleFavorite(USER_ID, RECIPE_ID);

        verify(bookItemRepository).existsByUserIdAndRecipeId(USER_ID, RECIPE_ID);
        verifyNoInteractions(likeRepository);
    }
}
