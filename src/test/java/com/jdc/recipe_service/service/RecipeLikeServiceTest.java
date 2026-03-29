package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeLike;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeLikeRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.UserRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeLikeServiceTest {

    @Mock
    private RecipeLikeRepository likeRepository;
    @Mock
    private RecipeRepository recipeRepository;
    @Mock
    private UserRepository userRepository;

    @InjectMocks
    private RecipeLikeService likeService;

    private User user;
    private Recipe recipe;

    @BeforeEach
    void setUp() {
        user = User.builder().id(1L).build();
        recipe = Recipe.builder().id(10L).build();
        ReflectionTestUtils.setField(recipe, "user", user);
    }

    @Test
    @DisplayName("toggleLike: 이미 좋아요 되어 있으면 삭제 후 false 반환")
    void toggleLike_existingLike_returnsFalse() {
        // Given
        RecipeLike existing = RecipeLike.builder()
                .id(200L)
                .user(user)
                .recipe(recipe)
                .build();
        given(likeRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.of(existing));
        willDoNothing().given(likeRepository).delete(existing);

        // When
        boolean result = likeService.toggleLike(user.getId(), recipe.getId());

        // Then
        assertThat(result).isFalse();
        verify(likeRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(likeRepository, times(1)).delete(existing);
        verifyNoInteractions(userRepository, recipeRepository);
    }

    @Test
    @DisplayName("toggleLike: 좋아요 기록이 없으면 새로 저장 후 true 반환")
    void toggleLike_noLike_savesAndReturnsTrue() {
        // Given
        given(likeRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.empty());
        given(userRepository.findById(user.getId())).willReturn(Optional.of(user));
        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.of(recipe));

        ArgumentCaptor<RecipeLike> captor = ArgumentCaptor.forClass(RecipeLike.class);
        given(likeRepository.save(captor.capture())).willAnswer(invocation -> invocation.getArgument(0));

        // When
        boolean result = likeService.toggleLike(user.getId(), recipe.getId());

        // Then
        assertThat(result).isTrue();
        verify(likeRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(likeRepository, times(1)).save(any(RecipeLike.class));

        RecipeLike saved = captor.getValue();
        assertThat(saved.getUser().getId()).isEqualTo(user.getId());
        assertThat(saved.getRecipe().getId()).isEqualTo(recipe.getId());
    }

    @Test
    @DisplayName("toggleLike: 존재하지 않는 유저면 USER_NOT_FOUND 예외")
    void toggleLike_userNotFound_throwsException() {
        // Given
        given(likeRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.empty());
        given(userRepository.findById(user.getId())).willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> likeService.toggleLike(user.getId(), recipe.getId()))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.USER_NOT_FOUND));

        verify(likeRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verifyNoMoreInteractions(recipeRepository, likeRepository);
    }

    @Test
    @DisplayName("toggleLike: 존재하지 않는 레시피면 RECIPE_NOT_FOUND 예외")
    void toggleLike_recipeNotFound_throwsException() {
        // Given
        given(likeRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.empty());
        given(userRepository.findById(user.getId())).willReturn(Optional.of(user));
        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> likeService.toggleLike(user.getId(), recipe.getId()))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.RECIPE_NOT_FOUND));

        verify(likeRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verifyNoMoreInteractions(likeRepository);
    }

    @Test
    @DisplayName("deleteByRecipeId: likeRepository.deleteByRecipeId 호출")
    void deleteByRecipeId_invokesRepository() {
        // Given
        willDoNothing().given(likeRepository).deleteByRecipeId(recipe.getId());

        // When
        likeService.deleteByRecipeId(recipe.getId());

        // Then
        verify(likeRepository, times(1)).deleteByRecipeId(recipe.getId());
    }
}
