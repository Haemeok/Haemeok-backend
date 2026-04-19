package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.ingredient.IngredientDetailDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.repository.RefrigeratorItemRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.querydsl.jpa.impl.JPAQueryFactory;
import org.hashids.Hashids;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

@ExtendWith(MockitoExtension.class)
class IngredientServiceTest {

    @Mock
    private JPAQueryFactory queryFactory;
    @Mock
    private IngredientRepository repo;
    @Mock
    private RefrigeratorItemRepository fridgeRepo;
    @Mock
    private RecipeRepository recipeRepository;
    @Mock
    private Hashids hashids;

    @InjectMocks
    private IngredientService ingredientService;

    @Test
    @DisplayName("findDetailById: 재료가 있고 레시피가 있으면 imageUrl/보관필드/recipes가 조립된 DTO 반환")
    void findDetailById_happyPath() {
        // given
        Long id = 42L;
        Ingredient ingredient = Ingredient.builder()
                .id(id)
                .name("대파")
                .category("채소")
                .storageLocation("냉장")
                .storageTemperature("0~4℃")
                .storageDuration("1~2주")
                .storageNotes("습기 주의")
                .goodPairs("돼지고기 / 마늘")
                .badPairs(null)
                .recommendedCookingMethods("구이 / 볶음")
                .build();
        RecipeSimpleDto recipe = RecipeSimpleDto.builder()
                .id(100L)
                .title("대파닭구이")
                .build();
        given(repo.findById(id)).willReturn(Optional.of(ingredient));
        given(recipeRepository.findTopByIngredientId(id, 10)).willReturn(List.of(recipe));

        // when
        IngredientDetailDto result = ingredientService.findDetailById(id);

        // then
        assertThat(result.getId()).isEqualTo(id);
        assertThat(result.getName()).isEqualTo("대파");
        assertThat(result.getCategory()).isEqualTo("채소");
        assertThat(result.getStorageLocation()).isEqualTo("냉장");
        assertThat(result.getStorageTemperature()).isEqualTo("0~4℃");
        assertThat(result.getStorageDuration()).isEqualTo("1~2주");
        assertThat(result.getStorageNotes()).isEqualTo("습기 주의");
        assertThat(result.getGoodPairs()).isEqualTo("돼지고기 / 마늘");
        assertThat(result.getBadPairs()).isNull();
        assertThat(result.getRecommendedCookingMethods()).isEqualTo("구이 / 볶음");
        assertThat(result.getImageUrl())
                .isEqualTo("https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/ingredients/대파.webp");
        assertThat(result.getRecipes()).containsExactly(recipe);
        verify(recipeRepository).findTopByIngredientId(id, 10);
    }

    @Test
    @DisplayName("findDetailById: 재료가 없으면 INGREDIENT_NOT_FOUND CustomException 발생하고 레시피 조회는 호출되지 않는다")
    void findDetailById_ingredientNotFound() {
        // given
        Long id = 999L;
        given(repo.findById(id)).willReturn(Optional.empty());

        // when & then
        assertThatThrownBy(() -> ingredientService.findDetailById(id))
                .isInstanceOf(CustomException.class)
                .extracting(e -> ((CustomException) e).getErrorCode())
                .isEqualTo(ErrorCode.INGREDIENT_NOT_FOUND);
        verifyNoInteractions(recipeRepository);
    }

    @Test
    @DisplayName("findDetailById: 해당 재료를 사용하는 레시피가 없으면 recipes는 빈 리스트(null 아님)")
    void findDetailById_noRecipes_returnsEmptyList() {
        // given
        Long id = 7L;
        Ingredient ingredient = Ingredient.builder()
                .id(id)
                .name("셀러리")
                .category("채소")
                .build();
        given(repo.findById(id)).willReturn(Optional.of(ingredient));
        given(recipeRepository.findTopByIngredientId(id, 10)).willReturn(Collections.emptyList());

        // when
        IngredientDetailDto result = ingredientService.findDetailById(id);

        // then
        assertThat(result.getRecipes()).isNotNull().isEmpty();
        assertThat(result.getStorageLocation()).isNull();
        assertThat(result.getRecommendedCookingMethods()).isNull();
    }
}
