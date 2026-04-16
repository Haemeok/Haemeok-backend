package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeFavorite;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.RecipeBookItemRepository;
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
    @Mock
    private RecipeBookItemRepository bookItemRepository;
    @Mock
    private RecipeBookService recipeBookService;

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
        RecipeFavorite existing = RecipeFavorite.builder()
                .id(200L)
                .user(user)
                .recipe(recipe)
                .build();
        given(favoriteRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.of(existing));
        willDoNothing().given(favoriteRepository).delete(existing);

        // When
        boolean result = favoriteService.toggleFavorite(user.getId(), recipe.getId());

        // Then
        assertThat(result).isFalse();
        verify(favoriteRepository).delete(existing);
    }

    @Test
    @DisplayName("toggleFavorite: 즐겨찾기 기록이 없으면 새로 저장 후 true 반환")
    void toggleFavorite_noFavorite_savesAndReturnsTrue() {
        // Given
        given(favoriteRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .willReturn(Optional.empty());
        given(userRepository.findById(user.getId())).willReturn(Optional.of(user));
        given(recipeRepository.findById(recipe.getId())).willReturn(Optional.of(recipe));

        ArgumentCaptor<RecipeFavorite> captor = ArgumentCaptor.forClass(RecipeFavorite.class);
        given(favoriteRepository.save(captor.capture()))
                .willAnswer(invocation -> invocation.getArgument(0));

        // When
        boolean result = favoriteService.toggleFavorite(user.getId(), recipe.getId());

        // Then
        assertThat(result).isTrue();
        verify(favoriteRepository).save(any(RecipeFavorite.class));

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
    }

    @Test
    @DisplayName("deleteByRecipeId: favorite 삭제 → count 보정 → bookItem 삭제 순서로 호출")
    void deleteByRecipeId_invokesInCorrectOrder() {
        // Given
        willDoNothing().given(favoriteRepository).deleteByRecipeId(recipe.getId());
        willDoNothing().given(recipeBookService).adjustCountsBeforeRecipeDeletion(recipe.getId());
        willDoNothing().given(bookItemRepository).deleteByRecipeId(recipe.getId());

        // When
        favoriteService.deleteByRecipeId(recipe.getId());

        // Then
        InOrder inOrder = inOrder(favoriteRepository, recipeBookService, bookItemRepository);
        inOrder.verify(favoriteRepository).deleteByRecipeId(recipe.getId());
        inOrder.verify(recipeBookService).adjustCountsBeforeRecipeDeletion(recipe.getId());
        inOrder.verify(bookItemRepository).deleteByRecipeId(recipe.getId());
    }
}
