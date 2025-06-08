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

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

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
        // 1) favoriteRepository.findByUserIdAndRecipeId(...) 가 Optional.of(...) 리턴
        RecipeFavorite existing = RecipeFavorite.builder()
                .id(100L)
                .user(user)
                .recipe(recipe)
                .build();
        when(favoriteRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .thenReturn(Optional.of(existing));

        // 2) delete(...) 호출 시 아무 일 없음
        doNothing().when(favoriteRepository).delete(existing);

        boolean result = favoriteService.toggleFavorite(user.getId(), recipe.getId());

        assertFalse(result);
        verify(favoriteRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(favoriteRepository, times(1)).delete(existing);
        // 유저/레시피 조회는 호출되지 않아야 함
        verifyNoInteractions(userRepository, recipeRepository);
    }

    @Test
    @DisplayName("toggleFavorite: 즐겨찾기 기록이 없으면 새로 저장 후 true 반환")
    void toggleFavorite_noFavorite_savesAndReturnsTrue() {
        // 1) findByUserIdAndRecipeId -> Optional.empty()
        when(favoriteRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .thenReturn(Optional.empty());

        // 2) userRepository.findById -> user
        when(userRepository.findById(user.getId())).thenReturn(Optional.of(user));
        // 3) recipeRepository.findById -> recipe
        when(recipeRepository.findById(recipe.getId())).thenReturn(Optional.of(recipe));

        // 4) save(...) 호출 시 그대로 RecipeFavorite 객체 리턴
        ArgumentCaptor<RecipeFavorite> captor = ArgumentCaptor.forClass(RecipeFavorite.class);
        when(favoriteRepository.save(captor.capture()))
                .thenAnswer(invocation -> invocation.getArgument(0));

        boolean result = favoriteService.toggleFavorite(user.getId(), recipe.getId());

        assertTrue(result);
        verify(favoriteRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(favoriteRepository, times(1)).save(any(RecipeFavorite.class));

        RecipeFavorite saved = captor.getValue();
        assertEquals(user.getId(), saved.getUser().getId());
        assertEquals(recipe.getId(), saved.getRecipe().getId());
    }

    @Test
    @DisplayName("toggleFavorite: 존재하지 않는 유저면 USER_NOT_FOUND 예외")
    void toggleFavorite_userNotFound_throwsException() {
        when(favoriteRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .thenReturn(Optional.empty());
        when(userRepository.findById(user.getId())).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () -> {
            favoriteService.toggleFavorite(user.getId(), recipe.getId());
        });
        assertEquals(ErrorCode.USER_NOT_FOUND, ex.getErrorCode());

        verify(favoriteRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verifyNoMoreInteractions(recipeRepository, favoriteRepository);
    }

    @Test
    @DisplayName("toggleFavorite: 존재하지 않는 레시피면 RECIPE_NOT_FOUND 예외")
    void toggleFavorite_recipeNotFound_throwsException() {
        when(favoriteRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .thenReturn(Optional.empty());
        when(userRepository.findById(user.getId())).thenReturn(Optional.of(user));
        when(recipeRepository.findById(recipe.getId())).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () -> {
            favoriteService.toggleFavorite(user.getId(), recipe.getId());
        });
        assertEquals(ErrorCode.RECIPE_NOT_FOUND, ex.getErrorCode());

        verify(favoriteRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verifyNoMoreInteractions(favoriteRepository);
    }

    @Test
    @DisplayName("deleteByRecipeId: favoriteRepository.deleteByRecipeId 호출")
    void deleteByRecipeId_invokesRepository() {
        doNothing().when(favoriteRepository).deleteByRecipeId(recipe.getId());

        favoriteService.deleteByRecipeId(recipe.getId());

        verify(favoriteRepository, times(1)).deleteByRecipeId(recipe.getId());
    }
}
