package com.jdc.recipe_service.dev.service.interaction;

import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeRating;
import com.jdc.recipe_service.domain.repository.RecipeRatingRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.service.RecipeRatingService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.anyDouble;
import static org.mockito.BDDMockito.given;
import static org.mockito.BDDMockito.willThrow;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * DevRatingService 분기 매트릭스.
 *
 * 핵심 invariant:
 *  - rate: pure update(existing + 빈 comment)는 운영 service 미호출 + gate 우회로 알림 누수 차단
 *  - rate: 신규 rating OR 새 comment → validator 통과 후에만 운영 service (알림은 게이트 통과한 add에만)
 *  - getMy/delete: self-data / cleanup right이라 게이트 없음
 */
@ExtendWith(MockitoExtension.class)
class DevRatingServiceTest {

    @Mock DevRecipeAccessValidator accessValidator;
    @Mock RecipeRatingRepository ratingRepository;
    @Mock RecipeRepository recipeRepository;
    @Mock RecipeRatingService ratingService;
    @Mock RecipeRating existingRating;
    @Mock Recipe recipe;

    @InjectMocks DevRatingService devRatingService;

    private static final Long USER_ID = 7L;
    private static final Long RECIPE_ID = 100L;

    // ---------- rateRecipe: pure update (existing + 빈 comment) ----------

    @Test
    @DisplayName("[rate] existing rating + comment=null → pure update: 운영 service 미호출, validator 미호출, rating 값/avg/count 직접 갱신")
    void rate_existingAndNoComment_pureUpdateBypassesGate() {
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(4.5).build();
        given(ratingRepository.findByUserIdAndRecipeId(USER_ID, RECIPE_ID))
                .willReturn(Optional.of(existingRating));
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(ratingRepository.calculateAverageByRecipeId(RECIPE_ID)).willReturn(4.5);
        given(ratingRepository.countByRecipeId(RECIPE_ID)).willReturn(3L);

        devRatingService.rateRecipe(USER_ID, RECIPE_ID, dto);

        verify(existingRating).updateRating(4.5);
        ArgumentCaptor<BigDecimal> avgCap = ArgumentCaptor.forClass(BigDecimal.class);
        verify(recipe).updateAvgRating(avgCap.capture());
        assertThat(avgCap.getValue()).isEqualByComparingTo(BigDecimal.valueOf(4.50));
        verify(recipe).updateRatingCount(3L);
        // 핵심 invariant: 운영 service와 validator 둘 다 미호출 → 알림 발생 자체 불가
        verifyNoInteractions(accessValidator, ratingService);
    }

    @Test
    @DisplayName("[rate] existing rating + comment=blank → pure update 경로 (whitespace는 빈 comment로 취급)")
    void rate_existingAndBlankComment_pureUpdate() {
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(3.0).comment("   ").build();
        given(ratingRepository.findByUserIdAndRecipeId(USER_ID, RECIPE_ID))
                .willReturn(Optional.of(existingRating));
        given(recipeRepository.findById(RECIPE_ID)).willReturn(Optional.of(recipe));
        given(ratingRepository.calculateAverageByRecipeId(RECIPE_ID)).willReturn(3.0);
        given(ratingRepository.countByRecipeId(RECIPE_ID)).willReturn(1L);

        devRatingService.rateRecipe(USER_ID, RECIPE_ID, dto);

        verify(existingRating).updateRating(3.0);
        verifyNoInteractions(accessValidator, ratingService);
    }

    @Test
    @DisplayName("[rate] pure update 경로에서 race로 rating이 이미 사라짐 → no-op (add 절대 안 함, 운영 service 미호출)")
    void rate_pureUpdate_raceRemovedNoOp() {
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(5.0).build();
        // findByUserAndRecipeId 첫 호출(pre-check)은 existing, 두번째 호출(updateExistingRatingDirect)은 race로 empty
        given(ratingRepository.findByUserIdAndRecipeId(USER_ID, RECIPE_ID))
                .willReturn(Optional.of(existingRating))    // pre-check
                .willReturn(Optional.empty());              // direct update 시점
        // recipe 조회는 안 가야 함

        devRatingService.rateRecipe(USER_ID, RECIPE_ID, dto);

        verify(existingRating, times(0)).updateRating(5.0);
        verifyNoInteractions(accessValidator, ratingService);
        verify(recipeRepository, times(0)).findById(RECIPE_ID);
    }

    // ---------- rateRecipe: 신규 또는 새 comment → 게이트 적용 ----------

    @Test
    @DisplayName("[rate] no existing rating + validator 통과 → 운영 ratingService에 위임 (신규 add 경로)")
    void rate_newRating_validatorPassDelegates() {
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(5.0).build();
        given(ratingRepository.findByUserIdAndRecipeId(USER_ID, RECIPE_ID))
                .willReturn(Optional.empty());

        devRatingService.rateRecipe(USER_ID, RECIPE_ID, dto);

        InOrder order = inOrder(accessValidator, ratingService);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        order.verify(ratingService).rateRecipe(RECIPE_ID, USER_ID, dto);
        // pure update 분기 미진입
        verify(existingRating, times(0)).updateRating(anyDouble());
    }

    @Test
    @DisplayName("[rate] existing rating + 새 comment → 게이트 적용 (comment 추가는 새 state라 알림 게이트 필요)")
    void rate_existingWithNewComment_validatorAppliedThenDelegates() {
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(4.0).comment("delicious!").build();
        given(ratingRepository.findByUserIdAndRecipeId(USER_ID, RECIPE_ID))
                .willReturn(Optional.of(existingRating));

        devRatingService.rateRecipe(USER_ID, RECIPE_ID, dto);

        InOrder order = inOrder(accessValidator, ratingService);
        order.verify(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);
        order.verify(ratingService).rateRecipe(RECIPE_ID, USER_ID, dto);
        // pure update 경로 미진입
        verify(existingRating, times(0)).updateRating(anyDouble());
    }

    @Test
    @DisplayName("[rate] no existing rating + validator throw → 운영 ratingService 미호출")
    void rate_newRating_validatorThrowsSkipsService() {
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(5.0).build();
        given(ratingRepository.findByUserIdAndRecipeId(USER_ID, RECIPE_ID))
                .willReturn(Optional.empty());
        willThrow(new CustomException(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devRatingService.rateRecipe(USER_ID, RECIPE_ID, dto))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_PRIVATE_ACCESS_DENIED);

        verifyNoInteractions(ratingService);
    }

    @Test
    @DisplayName("[rate] existing + 새 comment + validator throw → 운영 ratingService 미호출 (알림 누수 차단)")
    void rate_existingWithNewComment_validatorThrowsSkipsService() {
        RecipeRatingRequestDto dto = RecipeRatingRequestDto.builder().rating(4.0).comment("nope").build();
        given(ratingRepository.findByUserIdAndRecipeId(USER_ID, RECIPE_ID))
                .willReturn(Optional.of(existingRating));
        willThrow(new CustomException(ErrorCode.RECIPE_NOT_FOUND))
                .given(accessValidator).loadAndCheckInteractable(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devRatingService.rateRecipe(USER_ID, RECIPE_ID, dto))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RECIPE_NOT_FOUND);

        verifyNoInteractions(ratingService);
    }

    // ---------- getMyRating: 단순 위임 ----------

    @Test
    @DisplayName("[getMy] 게이트 없이 운영 ratingService.getMyRating에 위임 (self-data read)")
    void getMy_delegatesWithoutGate() {
        given(ratingService.getMyRating(RECIPE_ID, USER_ID)).willReturn(4.5);

        Double result = devRatingService.getMyRating(USER_ID, RECIPE_ID);

        assertThat(result).isEqualTo(4.5);
        verifyNoInteractions(accessValidator);
    }

    @Test
    @DisplayName("[getMy] rating 없으면 운영 동작대로 0.0 반환")
    void getMy_noRatingReturnsZero() {
        given(ratingService.getMyRating(RECIPE_ID, USER_ID)).willReturn(0.0);

        Double result = devRatingService.getMyRating(USER_ID, RECIPE_ID);

        assertThat(result).isEqualTo(0.0);
    }

    // ---------- deleteRating: 단순 위임 ----------

    @Test
    @DisplayName("[delete] 게이트 없이 운영 ratingService.deleteRating에 위임 (cleanup right)")
    void delete_delegatesWithoutGate() {
        devRatingService.deleteRating(USER_ID, RECIPE_ID);

        verify(ratingService).deleteRating(RECIPE_ID, USER_ID);
        verifyNoInteractions(accessValidator);
    }

    @Test
    @DisplayName("[delete] 운영 service가 RATING_NOT_FOUND throw하면 그대로 propagate")
    void delete_propagatesNotFound() {
        willThrow(new CustomException(ErrorCode.RATING_NOT_FOUND))
                .given(ratingService).deleteRating(RECIPE_ID, USER_ID);

        assertThatThrownBy(() -> devRatingService.deleteRating(USER_ID, RECIPE_ID))
                .isInstanceOf(CustomException.class)
                .extracting("errorCode")
                .isEqualTo(ErrorCode.RATING_NOT_FOUND);
    }

}
