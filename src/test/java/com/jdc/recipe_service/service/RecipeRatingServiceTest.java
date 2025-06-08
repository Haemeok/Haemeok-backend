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

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

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
    }

    @Test
    @DisplayName("rateRecipe: 새 평가 저장 후 반환")
    void rateRecipe_newRating_savesAndReturns() {
        RecipeRatingRequestDto dto = new RecipeRatingRequestDto(4.5, null);

        // 레시피/유저 조회 정상
        when(recipeRepository.findById(recipe.getId())).thenReturn(Optional.of(recipe));
        when(userRepository.findById(user.getId())).thenReturn(Optional.of(user));

        // ratingRepository.findByUserAndRecipe -> Optional.empty()
        when(ratingRepository.findByUserAndRecipe(user, recipe)).thenReturn(Optional.empty());

        // save(...) 호출 시 인자로 들어온 객체를 반환
        ArgumentCaptor<RecipeRating> captor = ArgumentCaptor.forClass(RecipeRating.class);
        when(ratingRepository.save(captor.capture()))
                .thenAnswer(invocation -> {
                    RecipeRating r = invocation.getArgument(0);
                    // 저장 후 ID 세팅
                    ReflectionTestUtils.setField(r, "id", 200L);
                    return r;
                });

        // 평균 계산용 stub (예시: 단건만 있어서 avg=4.5, count=1)
        when(ratingRepository.calculateAverageByRecipeId(recipe.getId())).thenReturn(4.5);
        when(ratingRepository.countByRecipeId(recipe.getId())).thenReturn(1L);

        RecipeRatingResponseDto response = ratingService.rateRecipe(recipe.getId(), user.getId(), dto);

        assertNotNull(response);
        assertEquals(200L, response.getId());
        assertEquals(4.5, response.getStars());

        // Repository 호출 검증
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(ratingRepository, times(1)).findByUserAndRecipe(user, recipe);
        verify(ratingRepository, times(1)).save(any(RecipeRating.class));

        // 평균 평점 갱신 검증
        assertEquals(BigDecimal.valueOf(4.5).setScale(2, RoundingMode.HALF_UP), recipe.getAvgRating());
        assertEquals(1L, recipe.getRatingCount());

        // 댓글 저장은 넘어간 상태 (dto.getComment()가 null)
        verify(commentRepository, never()).save(any(RecipeComment.class));

        // 등급 기록 생성 호출
        verify(cookingRecordService, times(1)).createRecordFromRating(user.getId(), recipe.getId(), 200L);
    }

    @Test
    @DisplayName("rateRecipe: 기존 평가가 있으면 평점만 업데이트")
    void rateRecipe_existingRating_updatesAndReturns() {
        RecipeRatingRequestDto dto = new RecipeRatingRequestDto(2.0, "맛있어요");

        when(recipeRepository.findById(recipe.getId())).thenReturn(Optional.of(recipe));
        when(userRepository.findById(user.getId())).thenReturn(Optional.of(user));
        // 기존 평가 있음
        when(ratingRepository.findByUserAndRecipe(user, recipe)).thenReturn(Optional.of(existingRating));

        // save(...) 시 그대로 existingRating 반환
        when(ratingRepository.save(existingRating)).thenReturn(existingRating);

        // 평균 및 카운트 계산: 예: (3 + 2) / 2 = 2.5
        when(ratingRepository.calculateAverageByRecipeId(recipe.getId())).thenReturn(2.5);
        when(ratingRepository.countByRecipeId(recipe.getId())).thenReturn(2L);

        RecipeRatingResponseDto response = ratingService.rateRecipe(recipe.getId(), user.getId(), dto);

        assertNotNull(response);
        assertEquals(existingRating.getId(), response.getId());
        assertEquals(2.0, response.getStars());

        // 기존 객체의 rating 필드가 변경됐는지
        assertEquals(2.0, existingRating.getRating());

        // 댓글 저장 검증 (dto.getComment()가 비어 있지 않으므로 저장)
        ArgumentCaptor<RecipeComment> commentCaptor = ArgumentCaptor.forClass(RecipeComment.class);
        verify(commentRepository, times(1)).save(commentCaptor.capture());

        RecipeComment savedComment = commentCaptor.getValue();
        assertEquals(user.getId(), savedComment.getUser().getId());
        assertEquals(recipe.getId(), savedComment.getRecipe().getId());
        assertEquals("맛있어요", savedComment.getComment());

        // 평균/카운트 갱신 검증
        assertEquals(BigDecimal.valueOf(2.5).setScale(2, RoundingMode.HALF_UP), recipe.getAvgRating());
        assertEquals(2L, recipe.getRatingCount());

        // 등급 기록 생성 호출 (ID는 existingRating의 ID)
        verify(cookingRecordService, times(1)).createRecordFromRating(user.getId(), recipe.getId(), existingRating.getId());
    }

    @Test
    @DisplayName("rateRecipe: 존재하지 않는 레시피면 RECIPE_NOT_FOUND 예외")
    void rateRecipe_recipeNotFound_throwsException() {
        RecipeRatingRequestDto dto = new RecipeRatingRequestDto(5.0, null);
        when(recipeRepository.findById(recipe.getId())).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () -> {
            ratingService.rateRecipe(recipe.getId(), user.getId(), dto);
        });
        assertEquals(ErrorCode.RECIPE_NOT_FOUND, ex.getErrorCode());

        verify(recipeRepository, times(1)).findById(recipe.getId());
        verifyNoMoreInteractions(userRepository, ratingRepository, commentRepository, cookingRecordService);
    }

    @Test
    @DisplayName("rateRecipe: 존재하지 않는 유저면 USER_NOT_FOUND 예외")
    void rateRecipe_userNotFound_throwsException() {
        RecipeRatingRequestDto dto = new RecipeRatingRequestDto(5.0, null);
        when(recipeRepository.findById(recipe.getId())).thenReturn(Optional.of(recipe));
        when(userRepository.findById(user.getId())).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () -> {
            ratingService.rateRecipe(recipe.getId(), user.getId(), dto);
        });
        assertEquals(ErrorCode.USER_NOT_FOUND, ex.getErrorCode());

        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verifyNoMoreInteractions(ratingRepository, commentRepository, cookingRecordService);
    }

    @Test
    @DisplayName("deleteRating: 정상 삭제 후 ID 반환")
    void deleteRating_existingRating_deletesAndReturnsId() {
        when(recipeRepository.findById(recipe.getId())).thenReturn(Optional.of(recipe));
        when(userRepository.findById(user.getId())).thenReturn(Optional.of(user));
        when(ratingRepository.findByUserAndRecipe(user, recipe)).thenReturn(Optional.of(existingRating));

        // delete(...) 시 아무 일 없음
        doNothing().when(ratingRepository).delete(existingRating);

        // 평균/카운트 갱신: 삭제 후 평가가 없어서 avg=0, count=0
        when(ratingRepository.calculateAverageByRecipeId(recipe.getId())).thenReturn(0.0);
        when(ratingRepository.countByRecipeId(recipe.getId())).thenReturn(0L);

        Long deletedId = ratingService.deleteRating(recipe.getId(), user.getId());

        assertEquals(existingRating.getId(), deletedId);
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(ratingRepository, times(1)).findByUserAndRecipe(user, recipe);
        verify(ratingRepository, times(1)).delete(existingRating);

        // 평균/카운트 재갱신 검증
        assertEquals(BigDecimal.valueOf(0.0).setScale(2, RoundingMode.HALF_UP), recipe.getAvgRating());
        assertEquals(0L, recipe.getRatingCount());
    }

    @Test
    @DisplayName("deleteRating: 평점 레코드가 없으면 RATING_NOT_FOUND 예외")
    void deleteRating_ratingNotFound_throwsException() {
        when(recipeRepository.findById(recipe.getId())).thenReturn(Optional.of(recipe));
        when(userRepository.findById(user.getId())).thenReturn(Optional.of(user));
        when(ratingRepository.findByUserAndRecipe(user, recipe)).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () -> {
            ratingService.deleteRating(recipe.getId(), user.getId());
        });
        assertEquals(ErrorCode.RATING_NOT_FOUND, ex.getErrorCode());

        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(ratingRepository, times(1)).findByUserAndRecipe(user, recipe);
        verifyNoMoreInteractions(ratingRepository);
    }

    @Test
    @DisplayName("getMyRating: 유저 평점 있으면 해당 평점 반환")
    void getMyRating_existingRating_returnsValue() {
        when(ratingRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .thenReturn(Optional.of(RecipeRating.builder().rating(3.7).build()));

        double result = ratingService.getMyRating(recipe.getId(), user.getId());
        assertEquals(3.7, result);
        verify(ratingRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
    }

    @Test
    @DisplayName("getMyRating: 평점이 없으면 0.0 반환")
    void getMyRating_noRating_returnsZero() {
        when(ratingRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .thenReturn(Optional.empty());

        double result = ratingService.getMyRating(recipe.getId(), user.getId());
        assertEquals(0.0, result);
        verify(ratingRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
    }

    @Test
    @DisplayName("getRatingCount: 레시피별 평점 개수 반환")
    void getRatingCount_returnsCount() {
        when(ratingRepository.countByRecipeId(recipe.getId())).thenReturn(5L);
        long count = ratingService.getRatingCount(recipe.getId());
        assertEquals(5L, count);
        verify(ratingRepository, times(1)).countByRecipeId(recipe.getId());
    }
}
