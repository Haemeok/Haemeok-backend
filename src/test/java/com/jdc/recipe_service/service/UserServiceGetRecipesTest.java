package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.MyRecipeSummaryDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeLike;
import com.jdc.recipe_service.domain.entity.User;
import com.jdc.recipe_service.domain.repository.*;
import com.jdc.recipe_service.domain.type.DishType;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.service.user.UserCreditService;
import com.jdc.recipe_service.util.S3Util;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class UserServiceGetRecipesTest {

    @Mock private UserRepository userRepository;
    @Mock private RecipeFavoriteRepository recipeFavoriteRepository;
    @Mock private RecipeLikeRepository recipeLikeRepository;
    @Mock private RecipeRepository recipeRepository;
    @Mock private S3Util s3Util;
    @Mock private DailyQuotaService dailyQuotaService;
    @Mock private UserCreditService userCreditService;

    @InjectMocks private UserService userService;

    private User owner;
    private User viewer;
    private Recipe aiRecipe;
    private Recipe userRecipe;
    private Recipe youtubeRecipe;
    private Pageable pageable;

    @BeforeEach
    void setUp() {
        owner = User.builder().id(1L).nickname("작성자").build();
        viewer = User.builder().id(2L).nickname("조회자").build();
        pageable = PageRequest.of(0, 10);

        aiRecipe = Recipe.builder()
                .id(10L).user(owner).title("AI 레시피")
                .dishType(DishType.FRYING).source(RecipeSourceType.AI)
                .isAiGenerated(true).isPrivate(false)
                .build();

        userRecipe = Recipe.builder()
                .id(11L).user(owner).title("유저 레시피")
                .dishType(DishType.SOUP_STEW).source(RecipeSourceType.USER)
                .isAiGenerated(false).isPrivate(false)
                .build();

        youtubeRecipe = Recipe.builder()
                .id(12L).user(owner).title("유튜브 레시피")
                .dishType(DishType.RICE_NOODLE).source(RecipeSourceType.YOUTUBE)
                .isAiGenerated(false).isPrivate(false)
                .build();
    }

    @Nested
    @DisplayName("본인 레시피 조회 (isOwner=true)")
    class OwnerRecipes {

        @Test
        @DisplayName("types 없음 → 전체 조회 (기존 쿼리 사용)")
        void noFilter_returnsAll() {
            // given
            given(userRepository.findById(1L)).willReturn(Optional.of(owner));
            Page<Recipe> allRecipes = new PageImpl<>(List.of(aiRecipe, userRecipe, youtubeRecipe), pageable, 3);
            given(recipeRepository.findCompletedRecipesByUserId(1L, pageable)).willReturn(allRecipes);
            given(recipeLikeRepository.findByUserIdAndRecipeIdIn(eq(1L), anyList()))
                    .willReturn(List.of());

            // when
            Page<MyRecipeSummaryDto> result = userService.getUserRecipes(1L, 1L, null, pageable);

            // then
            assertThat(result.getTotalElements()).isEqualTo(3);
            assertThat(result.getContent()).extracting(MyRecipeSummaryDto::getType)
                    .containsExactly("AI", "USER", "YOUTUBE");

            then(recipeRepository).should().findCompletedRecipesByUserId(1L, pageable);
            then(recipeRepository).should(never())
                    .findCompletedRecipesByUserIdAndSourceIn(anyLong(), anyList(), any());
        }

        @Test
        @DisplayName("types=AI,USER → 두 타입만 필터링")
        void filterAiAndUser_returnsTwoTypes() {
            // given
            List<RecipeSourceType> types = List.of(RecipeSourceType.AI, RecipeSourceType.USER);
            given(userRepository.findById(1L)).willReturn(Optional.of(owner));
            Page<Recipe> filtered = new PageImpl<>(List.of(aiRecipe, userRecipe), pageable, 2);
            given(recipeRepository.findCompletedRecipesByUserIdAndSourceIn(1L, types, pageable))
                    .willReturn(filtered);
            given(recipeLikeRepository.findByUserIdAndRecipeIdIn(eq(1L), anyList()))
                    .willReturn(List.of());

            // when
            Page<MyRecipeSummaryDto> result = userService.getUserRecipes(1L, 1L, types, pageable);

            // then
            assertThat(result.getTotalElements()).isEqualTo(2);
            assertThat(result.getContent()).extracting(MyRecipeSummaryDto::getType)
                    .containsExactly("AI", "USER");

            then(recipeRepository).should()
                    .findCompletedRecipesByUserIdAndSourceIn(1L, types, pageable);
            then(recipeRepository).should(never())
                    .findCompletedRecipesByUserId(anyLong(), any());
        }

        @Test
        @DisplayName("빈 리스트 types → 전체 조회로 fallback")
        void emptyList_returnsAll() {
            // given
            given(userRepository.findById(1L)).willReturn(Optional.of(owner));
            Page<Recipe> allRecipes = new PageImpl<>(List.of(aiRecipe, userRecipe, youtubeRecipe), pageable, 3);
            given(recipeRepository.findCompletedRecipesByUserId(1L, pageable)).willReturn(allRecipes);
            given(recipeLikeRepository.findByUserIdAndRecipeIdIn(eq(1L), anyList()))
                    .willReturn(List.of());

            // when
            Page<MyRecipeSummaryDto> result = userService.getUserRecipes(1L, 1L, List.of(), pageable);

            // then
            assertThat(result.getTotalElements()).isEqualTo(3);
            then(recipeRepository).should().findCompletedRecipesByUserId(1L, pageable);
        }
    }

    @Nested
    @DisplayName("타인 공개 레시피 조회 (isOwner=false)")
    class PublicRecipes {

        @Test
        @DisplayName("types 없음 → 공개 전체 조회")
        void noFilter_returnsAllPublic() {
            // given
            given(userRepository.findById(1L)).willReturn(Optional.of(owner));
            Page<Recipe> publicRecipes = new PageImpl<>(List.of(aiRecipe, userRecipe, youtubeRecipe), pageable, 3);
            given(recipeRepository.findByUserIdAndIsPrivateFalse(1L, pageable)).willReturn(publicRecipes);
            given(recipeLikeRepository.findByUserIdAndRecipeIdIn(eq(2L), anyList()))
                    .willReturn(List.of());

            // when
            Page<MyRecipeSummaryDto> result = userService.getUserRecipes(1L, 2L, null, pageable);

            // then
            assertThat(result.getTotalElements()).isEqualTo(3);
            then(recipeRepository).should().findByUserIdAndIsPrivateFalse(1L, pageable);
        }

        @Test
        @DisplayName("types=YOUTUBE → 유튜브 타입만 필터링")
        void filterYoutube_returnsOnlyYoutube() {
            // given
            List<RecipeSourceType> types = List.of(RecipeSourceType.YOUTUBE);
            given(userRepository.findById(1L)).willReturn(Optional.of(owner));
            Page<Recipe> filtered = new PageImpl<>(List.of(youtubeRecipe), pageable, 1);
            given(recipeRepository.findByUserIdAndIsPrivateFalseAndSourceIn(1L, types, pageable))
                    .willReturn(filtered);
            given(recipeLikeRepository.findByUserIdAndRecipeIdIn(eq(2L), anyList()))
                    .willReturn(List.of());

            // when
            Page<MyRecipeSummaryDto> result = userService.getUserRecipes(1L, 2L, types, pageable);

            // then
            assertThat(result.getTotalElements()).isEqualTo(1);
            assertThat(result.getContent().get(0).getType()).isEqualTo("YOUTUBE");

            then(recipeRepository).should()
                    .findByUserIdAndIsPrivateFalseAndSourceIn(1L, types, pageable);
        }
    }

    @Nested
    @DisplayName("응답 DTO 매핑")
    class DtoMapping {

        @Test
        @DisplayName("type 필드가 RecipeSourceType.name()으로 매핑된다")
        void typeFieldMappedFromSource() {
            // given
            given(userRepository.findById(1L)).willReturn(Optional.of(owner));
            Page<Recipe> recipes = new PageImpl<>(List.of(aiRecipe, userRecipe, youtubeRecipe), pageable, 3);
            given(recipeRepository.findCompletedRecipesByUserId(1L, pageable)).willReturn(recipes);
            given(recipeLikeRepository.findByUserIdAndRecipeIdIn(eq(1L), anyList()))
                    .willReturn(List.of());

            // when
            Page<MyRecipeSummaryDto> result = userService.getUserRecipes(1L, 1L, null, pageable);

            // then
            List<MyRecipeSummaryDto> content = result.getContent();
            assertThat(content.get(0).getType()).isEqualTo("AI");
            assertThat(content.get(1).getType()).isEqualTo("USER");
            assertThat(content.get(2).getType()).isEqualTo("YOUTUBE");
        }

        @Test
        @DisplayName("좋아요 상태가 likedByCurrentUser에 반영된다")
        void likedByCurrentUserReflected() {
            // given
            given(userRepository.findById(1L)).willReturn(Optional.of(owner));
            Page<Recipe> recipes = new PageImpl<>(List.of(aiRecipe, userRecipe), pageable, 2);
            given(recipeRepository.findCompletedRecipesByUserId(1L, pageable)).willReturn(recipes);

            RecipeLike like = RecipeLike.builder().user(owner).recipe(aiRecipe).build();
            given(recipeLikeRepository.findByUserIdAndRecipeIdIn(eq(1L), anyList()))
                    .willReturn(List.of(like));

            // when
            Page<MyRecipeSummaryDto> result = userService.getUserRecipes(1L, 1L, null, pageable);

            // then
            assertThat(result.getContent().get(0).isLikedByCurrentUser()).isTrue();
            assertThat(result.getContent().get(1).isLikedByCurrentUser()).isFalse();
        }
    }
}
