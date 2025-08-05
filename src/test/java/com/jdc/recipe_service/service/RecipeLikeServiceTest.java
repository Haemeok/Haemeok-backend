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

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

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
        RecipeLike existing = RecipeLike.builder()
                .id(200L)
                .user(user)
                .recipe(recipe)
                .build();
        when(likeRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .thenReturn(Optional.of(existing));
        doNothing().when(likeRepository).delete(existing);

        boolean result = likeService.toggleLike(user.getId(), recipe.getId());

        assertFalse(result);
        verify(likeRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(likeRepository, times(1)).delete(existing);
        verifyNoInteractions(userRepository, recipeRepository);
    }

    @Test
    @DisplayName("toggleLike: 좋아요 기록이 없으면 새로 저장 후 true 반환")
    void toggleLike_noLike_savesAndReturnsTrue() {
        when(likeRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .thenReturn(Optional.empty());
        when(userRepository.findById(user.getId())).thenReturn(Optional.of(user));
        when(recipeRepository.findById(recipe.getId())).thenReturn(Optional.of(recipe));

        ArgumentCaptor<RecipeLike> captor = ArgumentCaptor.forClass(RecipeLike.class);
        when(likeRepository.save(captor.capture())).thenAnswer(invocation -> invocation.getArgument(0));

        boolean result = likeService.toggleLike(user.getId(), recipe.getId());

        assertTrue(result);
        verify(likeRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verify(likeRepository, times(1)).save(any(RecipeLike.class));

        RecipeLike saved = captor.getValue();
        assertEquals(user.getId(), saved.getUser().getId());
        assertEquals(recipe.getId(), saved.getRecipe().getId());
    }

    @Test
    @DisplayName("toggleLike: 존재하지 않는 유저면 USER_NOT_FOUND 예외")
    void toggleLike_userNotFound_throwsException() {
        when(likeRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .thenReturn(Optional.empty());
        when(userRepository.findById(user.getId())).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () -> {
            likeService.toggleLike(user.getId(), recipe.getId());
        });
        assertEquals(ErrorCode.USER_NOT_FOUND, ex.getErrorCode());

        verify(likeRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verifyNoMoreInteractions(recipeRepository, likeRepository);
    }

    @Test
    @DisplayName("toggleLike: 존재하지 않는 레시피면 RECIPE_NOT_FOUND 예외")
    void toggleLike_recipeNotFound_throwsException() {
        when(likeRepository.findByUserIdAndRecipeId(user.getId(), recipe.getId()))
                .thenReturn(Optional.empty());
        when(userRepository.findById(user.getId())).thenReturn(Optional.of(user));
        when(recipeRepository.findById(recipe.getId())).thenReturn(Optional.empty());

        CustomException ex = assertThrows(CustomException.class, () -> {
            likeService.toggleLike(user.getId(), recipe.getId());
        });
        assertEquals(ErrorCode.RECIPE_NOT_FOUND, ex.getErrorCode());

        verify(likeRepository, times(1)).findByUserIdAndRecipeId(user.getId(), recipe.getId());
        verify(userRepository, times(1)).findById(user.getId());
        verify(recipeRepository, times(1)).findById(recipe.getId());
        verifyNoMoreInteractions(likeRepository);
    }

    @Test
    @DisplayName("deleteByRecipeId: likeRepository.deleteByRecipeId 호출")
    void deleteByRecipeId_invokesRepository() {
        doNothing().when(likeRepository).deleteByRecipeId(recipe.getId());

        likeService.deleteByRecipeId(recipe.getId());

        verify(likeRepository, times(1)).deleteByRecipeId(recipe.getId());
    }
}
