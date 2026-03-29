package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeFavoriteRepository;
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

import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeFavoriteServiceTest {

    @Mock
    private RecipeFavoriteRepository favoriteRepository;
    @Mock
    private RecipeRepository recipeRepository;
    @Mock
    private UserRepository userRepository;

    @InjectMocks
    private RecipeFavoriteService favoriteService;

    private User user;
    private Recipe recipe;

    @BeforeEach
    void setUp() {
        user = User.builder().id(1L).build();
        recipe = Recipe.builder().id(10L).build();
    }

    @Test
    @DisplayName("toggleFavorite: 이미 즐겨찾기 되어 있으면 삭제 후 false 반환")
    void toggleFavorite_existingFavorite_returnsFalse() {
        // Given
        // 1) favoriteRepository.findByUserIdAndRecipeId(...) 가 Optional.of(...) 리턴
        RecipeFavorite existing = RecipeFavorite.builder()
                .id(100L)
                .user(user)
                .recipe(recipe)
                .build();
        given(favoriteRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.of(existing));

        // 2) delete(...) 호출 시 아무 일 없음
        willDoNothing().given(favoriteRepository).delete(existing);

        // When
        boolean result = favoriteService.toggleFavorite(user.getId(), recipe.getId());

        // Then
        assertThat(result).isFalse();
        verify(favoriteRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(favoriteRepository, times(1)).delete(existing);
        // 유저/레시피 조회는 호출되지 않아야 함
        verifyNoInteractions(userRepository, recipeRepository);
    }

    @Test
    @DisplayName("toggleFavorite: 즐겨찾기 기록이 없으면 새로 저장 후 true 반환")
    void toggleFavorite_noFavorite_savesAndReturnsTrue() {
        // Given
        // 1) findByUserIdAndRecipeId -> Optional.empty()
        given(favoriteRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.empty());

        // 2) userRepository.findById -> user
        given(userRepository.findById(user.getId())).willReturn(Optional.of(user));
        // 3) recipeRepository.findById -> recipe
        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.of(recipe));

        // 4) save(...) 호출 시 그대로 RecipeFavorite 객체 리턴
        ArgumentCaptor<RecipeFavorite> captor = ArgumentCaptor.forClass(RecipeFavorite.class);
        given(favoriteRepository.save(captor.capture()))
                .willAnswer(invocation -> invocation.getArgument(0));

        // When
        boolean result = favoriteService.toggleFavorite(user.getId(), recipe.getId());

        // Then
        assertThat(result).isTrue();
        verify(favoriteRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(favoriteRepository, times(1)).save(any(RecipeFavorite.class));

        RecipeFavorite saved = captor.getValue();
        assertThat(saved.getUser().getId()).isEqualTo(user.getId());
        assertThat(saved.getRecipe().getId()).isEqualTo(recipe.getId());
    }

    @Test
    @DisplayName("toggleFavorite: 존재하지 않는 유저면 USER_NOT_FOUND 예외")
    void toggleFavorite_userNotFound_throwsException() {
        // Given
        given(favoriteRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.empty());
        given(userRepository.findById(user.getId())).willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> favoriteService.toggleFavorite(user.getId(), recipe.getId()))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.USER_NOT_FOUND));

        verify(favoriteRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verifyNoMoreInteractions(recipeRepository, favoriteRepository);
    }

    @Test
    @DisplayName("toggleFavorite: 존재하지 않는 레시피면 RECIPE_NOT_FOUND 예외")
    void toggleFavorite_recipeNotFound_throwsException() {
        // Given
        given(favoriteRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.empty());
        given(userRepository.findById(user.getId())).willReturn(Optional.of(user));
        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.empty());

        // When & Then
        assertThatThrownBy(() -> favoriteService.toggleFavorite(user.getId(), recipe.getId()))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.RECIPE_NOT_FOUND));

        verify(favoriteRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verifyNoMoreInteractions(favoriteRepository);
    }

    @Test
    @DisplayName("deleteByRecipeId: favoriteRepository.deleteByRecipeId 호출")
    void deleteByRecipeId_invokesRepository() {
        // Given
        willDoNothing().given(favoriteRepository).deleteByRecipeId(recipe.getId());

        // When
        favoriteService.deleteByRecipeId(recipe.getId());

        // Then
        verify(favoriteRepository, times(1)).deleteByRecipeId(recipe.getId());
    }
}
