package com.jdc.recipe_service.dev.service.interaction;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeComment;
import com.jdc.recipe_service.domain.repository.CommentLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeCommentRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.CommentLikeService;
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
 * DevCommentLikeService 분기 매트릭스.
 *
 *  - 기존 like 있음 → 게이트 우회, 운영 service 위임 (cleanup right)
 *  - 기존 like 없음 + validator 통과 → 운영 service에 위임 (신규 add)
 *  - 기존 like 없음 + validator throw → 운영 service 미호출
 *  - race recovery: pre-check 우회 + delegate add(true) → post-check 호출
 *  - countLikes는 운영 service 단순 위임
 */
@ExtendWith(MockitoExtension.class)
class DevCommentLikeServiceTest {

    @Mock DevRecipeAccessValidator accessValidator;
    @Mock CommentLikeRepository commentLikeRepository;
    @Mock RecipeCommentRepository recipeCommentRepository;
    @Mock CommentLikeService commentLikeService;
    @Mock RecipeComment comment;
    @Mock Recipe recipe;

    @InjectMocks DevCommentLikeService devCommentLikeService;

    private static final Long USER_ID = 7L;
    private static final Long COMMENT_ID = 200L;
    private static final Long RECIPE_ID = 100L;

    // ---------- 기존 like 있음 ----------

    @Test
    @DisplayName("[existing] 기존 like 있음 + delegate가 false (정상 제거) → validator 우회 (cleanup)")
    void existing_normalRemove_bypassesValidator() {
        given(commentLikeRepository.existsByCommentIdAndUserId(COMMENT_ID, USER_ID)).willReturn(true);
        given(commentLikeService.toggleLike(COMMENT_ID, USER_ID)).willReturn(false);

        boolean result = devCommentLikeService.toggleLike(USER_ID, COMMENT_ID);

        assertThat(result).isFalse();
        verify(commentLikeService).toggleLike(COMMENT_ID, USER_ID);
        verifyNoInteractions(accessValidator, recipeCommentRepository);
    }

    // ---------- 신규 like ----------

    @Test
    @DisplayName("[new] validator 통과 → comment → recipe 추적 후 운영 service 위임 → true")
    void new_validatorPass_delegates() {
        given(commentLikeRepository.existsByCommentIdAndUserId(COMMENT_ID, USER_ID)).willReturn(false);
        given(recipeCommentRepository.findById(COMMENT_ID)).willReturn(Optional.of(comment));
        given(comment.getRecipe()).willReturn(recipe);
        given(recipe.getId()).willReturn(RECIPE_ID);
        given(commentLikeService.toggleLike(COMMENT_ID, USER_ID)).willReturn(true);

        boolean result = devCommentLikeService.toggleLike(USER_ID, COMMENT_ID);

        assertThat(result).isTrue();
        InOrder order = inOrder(accessValidator, commentLikeService);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        order.verify(commentLikeService).toggleLike(COMMENT_ID, USER_ID);
        // pre-check 했으므로 post-check 추가 호출 없음
        verify(accessValidator, times(1)).loadAndCheckInteractable(RECIPE_ID, USER_ID);
    }

    @Test
    @DisplayName("[new] validator throw → 운영 service 미호출 (RESTRICTED 댓글 like 차단)")
    void new_validatorThrow_skipsOperationalService() {
        given(commentLikeRepository.existsByCommentIdAndUserId(COMMENT_ID, USER_ID)).willReturn(false);
        given(recipeCommentRepository.findById(COMMENT_ID)).willReturn(Optional.of(comment));
        given(comment.getRecipe()).willReturn(recipe);
        given(recipe.getId()).willReturn(RECIPE_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devCommentLikeService.toggleLike(USER_ID, COMMENT_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(commentLikeService);
    }

    @Test
    @DisplayName("[new] comment 없음 → COMMENT_NOT_FOUND, 운영 service 미호출")
    void new_commentNotFound_throws() {
        given(commentLikeRepository.existsByCommentIdAndUserId(COMMENT_ID, USER_ID)).willReturn(false);
        given(recipeCommentRepository.findById(COMMENT_ID)).willReturn(Optional.empty());

        assertThatThrownBy(() -> devCommentLikeService.toggleLike(USER_ID, COMMENT_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.COMMENT_NOT_FOUND);

        verifyNoInteractions(accessValidator, commentLikeService);
    }

    // ---------- race recovery (post-check) ----------

    @Test
    @DisplayName("[race] pre-check=existing이지만 delegate가 add(true) → post-check 호출 → throw 시 propagate")
    void race_preCheckExistingButDelegateAdded_postCheckThrows() {
        // pre-check 시점엔 existing이지만 race로 사라진 후 delegate가 새로 add → liked=true
        given(commentLikeRepository.existsByCommentIdAndUserId(COMMENT_ID, USER_ID)).willReturn(true);
        given(commentLikeService.toggleLike(COMMENT_ID, USER_ID)).willReturn(true);
        // post-check를 위한 comment 조회
        given(recipeCommentRepository.findById(COMMENT_ID)).willReturn(Optional.of(comment));
        given(comment.getRecipe()).willReturn(recipe);
        given(recipe.getId()).willReturn(RECIPE_ID);
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devCommentLikeService.toggleLike(USER_ID, COMMENT_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        // pre-check 우회됨 + post-check 1회 호출 (운영 service insert는 outer @Transactional rollback)
        verify(accessValidator, times(1)).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        verify(commentLikeService).toggleLike(COMMENT_ID, USER_ID);
    }

    @Test
    @DisplayName("[race] pre-check=existing + delegate add(true) + post-check 통과 (PUBLIC) → true 반환")
    void race_preCheckExistingButDelegateAdded_postCheckPasses() {
        given(commentLikeRepository.existsByCommentIdAndUserId(COMMENT_ID, USER_ID)).willReturn(true);
        given(commentLikeService.toggleLike(COMMENT_ID, USER_ID)).willReturn(true);
        given(recipeCommentRepository.findById(COMMENT_ID)).willReturn(Optional.of(comment));
        given(comment.getRecipe()).willReturn(recipe);
        given(recipe.getId()).willReturn(RECIPE_ID);
        // validator는 silently 통과

        boolean result = devCommentLikeService.toggleLike(USER_ID, COMMENT_ID);

        assertThat(result).isTrue();
        verify(accessValidator, times(1)).loadAndCheckInteractable(RECIPE_ID, USER_ID);
    }

    // ---------- countLikes ----------

    @Test
    @DisplayName("[count] 운영 commentLikeService.countLikes에 단순 위임")
    void countLikes_delegates() {
        given(commentLikeService.countLikes(COMMENT_ID)).willReturn(42);

        int result = devCommentLikeService.countLikes(COMMENT_ID);

        assertThat(result).isEqualTo(42);
    }
}
