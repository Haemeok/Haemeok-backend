package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeRatingResponseDto;
import com.jdc.recipe_service.domain.entity.*;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeRatingServiceTest {

    @Mock
    private RecipeRatingRepository ratingRepository;
    @Mock
    private RecipeRepository recipeRepository;
    @Mock
    private UserRepository userRepository;
    @Mock
    private RecipeCommentRepository commentRepository;
    @Mock
    private CookingRecordService cookingRecordService;

    @InjectMocks
    private RecipeRatingService ratingService;

    private User user;
    private Recipe recipe;
    private RecipeRating existingRating;

    @BeforeEach
    void setUp() {
        user = User.builder().id(1L).build();
        recipe = Recipe.builder().id(10L).avgRating(BigDecimal.ZERO).ratingCount(0L).build();
        existingRating = RecipeRating.builder()
                .id(100L)
                .user(user)
                .recipe(recipe)
                .rating(3.0)
                .build();
        ReflectionTestUtils.setField(recipe, "user", user);
    }

    @Test
    @DisplayName("rateRecipe: 새 평가 저장 후 반환하는 테스트")
    void rateRecipe_newRating_savesAndReturns() {
        // Given
        RecipeRatingRequestDto dto = new RecipeRatingRequestDto(4.5, null);

        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.of(recipe));
        given(userRepository.findById(user.getId())).willReturn(Optional.of(user));
        given(ratingRepository.findByUserAndRecipe(user, recipe)).willReturn(Optional.empty());

        ArgumentCaptor<RecipeRating> captor = ArgumentCaptor.forClass(RecipeRating.class);
        given(ratingRepository.save(captor.capture()))
                .willAnswer(invocation -> {
                    RecipeRating r = invocation.getArgument(0);
                    // 저장 후 ID 세팅
                    ReflectionTestUtils.setField(r, "id", 200L);
                    return r;
                });

        given(ratingRepository.calculateAverageByRecipeId(recipe.getId())).willReturn(4.5);
        given(ratingRepository.countByRecipeId(recipe.getId())).willReturn(1L);

        // When
        RecipeRatingResponseDto response = ratingService.rateRecipe(recipe.getId(), user.getId(), dto);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getId()).isEqualTo(200L);
        assertThat(response.getStars()).isEqualTo(4.5);

        // Repository 호출 검증
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(ratingRepository, times(1)).findByUserAndRecipe(user, recipe);
        verify(ratingRepository, times(1)).save(any(RecipeRating.class));

        // 평균 평점 갱신 검증
        assertThat(recipe.getAvgRating()).isEqualTo(BigDecimal.valueOf(4.5).setScale(2, RoundingMode.HALF_UP));
        assertThat(recipe.getRatingCount()).isEqualTo(1L);

        // 댓글 저장은 넘어간 상태 (dto.getComment()가 null)
        verify(commentRepository, never()).save(any(RecipeComment.class));

    }

    @Test
    @DisplayName("rateRecipe: 기존 평가가 있으면 평점만 업데이트하는 테스트")
    void rateRecipe_existingRating_updatesAndReturns() {
        // Given
        RecipeRatingRequestDto dto = new RecipeRatingRequestDto(2.0, "맛있어요");

        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.of(recipe));
        given(userRepository.findById(user.getId())).willReturn(Optional.of(user));
        // 기존 평가 있음
        given(ratingRepository.findByUserAndRecipe(user, recipe)).willReturn(Optional.of(existingRating));

        // save(...) 시 그대로 existingRating 반환
        given(ratingRepository.save(existingRating)).willReturn(existingRating);

        // 평균 및 카운트 계산: 예: (3 + 2) / 2 = 2.5
        given(ratingRepository.calculateAverageByRecipeId(recipe.getId())).willReturn(2.5);
        given(ratingRepository.countByRecipeId(recipe.getId())).willReturn(2L);

        // When
        RecipeRatingResponseDto response = ratingService.rateRecipe(recipe.getId(), user.getId(), dto);

        // Then
        assertThat(response).isNotNull();
        assertThat(response.getId()).isEqualTo(existingRating.getId());
        assertThat(response.getStars()).isEqualTo(2.0);

        // 기존 객체의 rating 필드가 변경됐는지
        assertThat(existingRating.getRating()).isEqualTo(2.0);

        // 댓글 저장 검증 (dto.getComment()가 비어 있지 않으므로 저장)
        ArgumentCaptor<RecipeComment> commentCaptor = ArgumentCaptor.forClass(RecipeComment.class);
        verify(commentRepository, times(1)).save(commentCaptor.capture());

        RecipeComment savedComment = commentCaptor.getValue();
        assertThat(savedComment.getUser().getId()).isEqualTo(user.getId());
        assertThat(savedComment.getRecipe().getId()).isEqualTo(recipe.getId());
        assertThat(savedComment.getComment()).isEqualTo("맛있어요");

        // 평균/카운트 갱신 검증
        assertThat(recipe.getAvgRating()).isEqualTo(BigDecimal.valueOf(2.5).setScale(2, RoundingMode.HALF_UP));
        assertThat(recipe.getRatingCount()).isEqualTo(2L);

    }

    @Test
    @DisplayName("rateRecipe: 존재하지 않는 레시피면 RECIPE_NOT_FOUND 예외 발생하는 테스트")
    void rateRecipe_recipeNotFound_throwsException() {
        // Given
        RecipeRatingRequestDto dto = new RecipeRatingRequestDto(5.0, null);
        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> ratingService.rateRecipe(recipe.getId(), user.getId(), dto))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.RECIPE_NOT_FOUND));

        verify(recipeRepository, times(1)).findById(recipe.getId());
        verifyNoMoreInteractions(userRepository, ratingRepository, commentRepository, cookingRecordService);
    }

    @Test
    @DisplayName("rateRecipe: 존재하지 않는 유저면 USER_NOT_FOUND 예외 발생하는 테스트")
    void rateRecipe_userNotFound_throwsException() {
        // Given
        RecipeRatingRequestDto dto = new RecipeRatingRequestDto(5.0, null);
        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.of(recipe));
        given(userRepository.findById(user.getId())).willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> ratingService.rateRecipe(recipe.getId(), user.getId(), dto))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.USER_NOT_FOUND));

        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verifyNoMoreInteractions(ratingRepository, commentRepository, cookingRecordService);
    }

    @Test
    @DisplayName("deleteRating: 정상 삭제 후 ID 반환하는 테스트")
    void deleteRating_existingRating_deletesAndReturnsId() {
        // Given
        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.of(recipe));
        given(userRepository.findById(user.getId())).willReturn(Optional.of(user));
        given(ratingRepository.findByUserAndRecipe(user, recipe)).willReturn(Optional.of(existingRating));

        // delete(...) 시 아무 일 없음
        willDoNothing().given(ratingRepository).delete(existingRating);

        // 평균/카운트 갱신: 삭제 후 평가가 없어서 avg=0, count=0
        given(ratingRepository.calculateAverageByRecipeId(recipe.getId())).willReturn(0.0);
        given(ratingRepository.countByRecipeId(recipe.getId())).willReturn(0L);

        // When
        Long deletedId = ratingService.deleteRating(recipe.getId(), user.getId());

        // Then
        assertThat(deletedId).isEqualTo(existingRating.getId());
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(ratingRepository, times(1)).findByUserAndRecipe(user, recipe);
        verify(ratingRepository, times(1)).delete(existingRating);

        // 평균/카운트 재갱신 검증
        assertThat(recipe.getAvgRating()).isEqualTo(BigDecimal.valueOf(0.0).setScale(2, RoundingMode.HALF_UP));
        assertThat(recipe.getRatingCount()).isEqualTo(0L);
    }

    @Test
    @DisplayName("deleteRating: 평점 레코드가 없으면 RATING_NOT_FOUND 예외 발생하는 테스트")
    void deleteRating_ratingNotFound_throwsException() {
        // Given
        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.of(recipe));
        given(userRepository.findById(user.getId())).willReturn(Optional.of(user));
        given(ratingRepository.findByUserAndRecipe(user, recipe)).willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> ratingService.deleteRating(recipe.getId(), user.getId()))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.RATING_NOT_FOUND));

        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(ratingRepository, times(1)).findByUserAndRecipe(user, recipe);
        verifyNoMoreInteractions(ratingRepository);
    }

    @Test
    @DisplayName("getMyRating: 유저 평점 있으면 해당 평점 반환하는 테스트")
    void getMyRating_existingRating_returnsValue() {
        // Given
        given(ratingRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.of(RecipeRating.builder().rating(3.7).build()));

        // When
        double result = ratingService.getMyRating(recipe.getId(), user.getId());

        // Then
        assertThat(result).isEqualTo(3.7);
        verify(ratingRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
    }

    @Test
    @DisplayName("getMyRating: 평점이 없으면 0.0 반환하는 테스트")
    void getMyRating_noRating_returnsZero() {
        // Given
        given(ratingRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.empty());

        // When
        double result = ratingService.getMyRating(recipe.getId(), user.getId());

        // Then
        assertThat(result).isEqualTo(0.0);
        verify(ratingRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
    }

    @Test
    @DisplayName("getRatingCount: 레시피별 평점 개수 반환하는 테스트")
    void getRatingCount_returnsCount() {
        // Given
        given(ratingRepository.countByRecipeId(recipe.getId())).willReturn(5L);

        // When
        long count = ratingService.getRatingCount(recipe.getId());

        // Then
        assertThat(count).isEqualTo(5L);
        verify(ratingRepository, times(1)).countByRecipeId(recipe.getId());
    }
}
