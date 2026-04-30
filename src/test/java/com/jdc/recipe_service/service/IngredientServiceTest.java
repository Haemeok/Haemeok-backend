package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.ingredient.IngredientDetailDto;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientUnitsBatchResponse;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientUnitsResponse;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
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

import java.math.BigDecimal;
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
    private IngredientUnitRepository ingredientUnitRepository;
    @Mock
    private RefrigeratorItemRepository fridgeRepo;
    @Mock
    private RecipeRepository recipeRepository;
    @Mock
    private Hashids hashids;

    @InjectMocks
    private IngredientService ingredientService;

    @Test
    @DisplayName("findDetailById: returns storage, nutrition per 100g, structured pairs, metadata, and top recipes")
    void findDetailById_happyPath() {
        Long id = 42L;
        Ingredient ingredient = Ingredient.builder()
                .id(id)
                .name("potato")
                .category("vegetable")
                .coupangLink("https://link.coupang.com/a/potato")
                .storageLocation("fridge")
                .storageTemperature("0~4C")
                .storageDuration("1~2 weeks")
                .storageNotes("keep dry")
                .kcalPerG(new BigDecimal("0.770000"))
                .carbohydrateGPerG(new BigDecimal("0.175000"))
                .proteinGPerG(new BigDecimal("0.020000"))
                .fatGPerG(new BigDecimal("0.001000"))
                .sugarGPerG(new BigDecimal("0.008000"))
                .sodiumMgPerG(new BigDecimal("0.060000"))
                .goodPairs("Pork / Garlic / Unknown")
                .badPairs("Milk")
                .benefits("Rich in vitamin C")
                .seasonMonths(List.of(6, 7, 8))
                .recommendedCookingMethods("roast / fry")
                .build();
        Ingredient garlic = Ingredient.builder()
                .id(201L)
                .name("Garlic")
                .build();
        Ingredient milk = Ingredient.builder()
                .id(202L)
                .name("Milk")
                .build();
        RecipeSimpleDto recipe = RecipeSimpleDto.builder()
                .id(100L)
                .title("potato roast")
                .build();

        given(repo.findById(id)).willReturn(Optional.of(ingredient));
        given(repo.findAllByNameIn(List.of("Pork", "Garlic", "Unknown", "Milk")))
                .willReturn(List.of(garlic, milk));
        given(recipeRepository.findTopByIngredientId(id, 10)).willReturn(List.of(recipe));

        IngredientDetailDto result = ingredientService.findDetailById(id);

        assertThat(result.getId()).isEqualTo(id);
        assertThat(result.getName()).isEqualTo("potato");
        assertThat(result.getCategory()).isEqualTo("vegetable");
        assertThat(result.getCoupangLink()).isEqualTo("https://link.coupang.com/a/potato");
        assertThat(result.getStorageLocation()).isEqualTo("fridge");
        assertThat(result.getStorageTemperature()).isEqualTo("0~4C");
        assertThat(result.getStorageDuration()).isEqualTo("1~2 weeks");
        assertThat(result.getStorageNotes()).isEqualTo("keep dry");
        assertThat(result.getImageUrl())
                .isEqualTo("https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/ingredients/potato.webp");
        assertThat(result.getNutritionPer100g().getKcal()).isEqualByComparingTo("77");
        assertThat(result.getNutritionPer100g().getCarbohydrateG()).isEqualByComparingTo("17.5");
        assertThat(result.getNutritionPer100g().getProteinG()).isEqualByComparingTo("2");
        assertThat(result.getNutritionPer100g().getFatG()).isEqualByComparingTo("0.1");
        assertThat(result.getNutritionPer100g().getSugarG()).isEqualByComparingTo("0.8");
        assertThat(result.getNutritionPer100g().getSodiumMg()).isEqualByComparingTo("6");
        assertThat(result.getGoodPairs()).isEqualTo("Pork / Garlic / Unknown");
        assertThat(result.getGoodPairItems()).hasSize(3);
        assertThat(result.getGoodPairItems().get(0).getName()).isEqualTo("Pork");
        assertThat(result.getGoodPairItems().get(0).getId()).isNull();
        assertThat(result.getGoodPairItems().get(1).getId()).isEqualTo(201L);
        assertThat(result.getGoodPairItems().get(1).getImageUrl())
                .isEqualTo("https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/ingredients/Garlic.webp");
        assertThat(result.getGoodPairItems().get(2).getName()).isEqualTo("Unknown");
        assertThat(result.getGoodPairItems().get(2).getId()).isNull();
        assertThat(result.getBadPairs()).isEqualTo("Milk");
        assertThat(result.getBadPairItems()).hasSize(1);
        assertThat(result.getBadPairItems().get(0).getId()).isEqualTo(202L);
        assertThat(result.getBadPairItems().get(0).getImageUrl())
                .isEqualTo("https://haemeok-s3-bucket.s3.ap-northeast-2.amazonaws.com/images/ingredients/Milk.webp");
        assertThat(result.getBenefits()).isEqualTo("Rich in vitamin C");
        assertThat(result.getSeasonMonths()).containsExactly(6, 7, 8);
        assertThat(result.getRecommendedCookingMethods()).isEqualTo("roast / fry");
        assertThat(result.getRecipes()).containsExactly(recipe);
        verify(recipeRepository).findTopByIngredientId(id, 10);
    }

    @Test
    @DisplayName("findDetailById: throws INGREDIENT_NOT_FOUND and skips recipe lookup when missing")
    void findDetailById_ingredientNotFound() {
        Long id = 999L;
        given(repo.findById(id)).willReturn(Optional.empty());

        assertThatThrownBy(() -> ingredientService.findDetailById(id))
                .isInstanceOf(CustomException.class)
                .extracting(e -> ((CustomException) e).getErrorCode())
                .isEqualTo(ErrorCode.INGREDIENT_NOT_FOUND);
        verifyNoInteractions(recipeRepository);
    }

    @Test
    @DisplayName("findDetailById: returns empty recipes and empty pair item lists when no recipe or pair data exists")
    void findDetailById_noRecipes_returnsEmptyList() {
        Long id = 7L;
        Ingredient ingredient = Ingredient.builder()
                .id(id)
                .name("celery")
                .category("vegetable")
                .build();
        given(repo.findById(id)).willReturn(Optional.of(ingredient));
        given(recipeRepository.findTopByIngredientId(id, 10)).willReturn(Collections.emptyList());

        IngredientDetailDto result = ingredientService.findDetailById(id);

        assertThat(result.getRecipes()).isNotNull().isEmpty();
        assertThat(result.getGoodPairItems()).isEmpty();
        assertThat(result.getBadPairItems()).isEmpty();
        assertThat(result.getStorageLocation()).isNull();
        assertThat(result.getRecommendedCookingMethods()).isNull();
    }

    @Test
    @DisplayName("findUnitsByIngredientId: returns default unit first with display grams")
    void findUnitsByIngredientId_returnsUnits() {
        Long ingredientId = 10L;
        Ingredient ingredient = Ingredient.builder().id(ingredientId).name("potato").build();
        IngredientUnit gram = IngredientUnit.builder()
                .id(101L)
                .ingredient(ingredient)
                .unitLabelKo("g")
                .gramsPerUnit(new BigDecimal("1.000"))
                .isDefault(false)
                .build();
        IngredientUnit piece = IngredientUnit.builder()
                .id(102L)
                .ingredient(ingredient)
                .unitLabelKo("개")
                .gramsPerUnit(new BigDecimal("150.000"))
                .isDefault(true)
                .build();

        given(repo.existsById(ingredientId)).willReturn(true);
        given(ingredientUnitRepository.findAllByIngredientIdIn(List.of(ingredientId)))
                .willReturn(List.of(gram, piece));

        IngredientUnitsResponse result = ingredientService.findUnitsByIngredientId(ingredientId);

        assertThat(result.getIngredientId()).isEqualTo(ingredientId);
        assertThat(result.getUnits()).hasSize(2);
        assertThat(result.getUnits().get(0).getUnit()).isEqualTo("개");
        assertThat(result.getUnits().get(0).getGramsPerUnit()).isEqualByComparingTo("150.000");
        assertThat(result.getUnits().get(0).getIsDefault()).isTrue();
        assertThat(result.getUnits().get(1).getUnit()).isEqualTo("g");
        verify(ingredientUnitRepository).findAllByIngredientIdIn(List.of(ingredientId));
    }

    @Test
    @DisplayName("findUnitsByIngredientId: throws INGREDIENT_NOT_FOUND when ingredient does not exist")
    void findUnitsByIngredientId_notFound() {
        Long ingredientId = 999L;
        given(repo.existsById(ingredientId)).willReturn(false);

        assertThatThrownBy(() -> ingredientService.findUnitsByIngredientId(ingredientId))
                .isInstanceOf(CustomException.class)
                .extracting(e -> ((CustomException) e).getErrorCode())
                .isEqualTo(ErrorCode.INGREDIENT_NOT_FOUND);
        verifyNoInteractions(ingredientUnitRepository);
    }

    @Test
    @DisplayName("findUnitsByIngredientIds: preserves request order after de-duplication and groups units")
    void findUnitsByIngredientIds_preservesOrderAndGroups() {
        Long potatoId = 10L;
        Long garlicId = 20L;
        Long noUnitId = 30L;
        Ingredient potato = Ingredient.builder().id(potatoId).name("potato").build();
        Ingredient garlic = Ingredient.builder().id(garlicId).name("garlic").build();
        Ingredient noUnit = Ingredient.builder().id(noUnitId).name("salt").build();
        IngredientUnit potatoUnit = IngredientUnit.builder()
                .id(101L)
                .ingredient(potato)
                .unitLabelKo("개")
                .gramsPerUnit(new BigDecimal("150.000"))
                .isDefault(true)
                .build();
        IngredientUnit garlicUnit = IngredientUnit.builder()
                .id(201L)
                .ingredient(garlic)
                .unitLabelKo("쪽")
                .gramsPerUnit(new BigDecimal("5.000"))
                .isDefault(true)
                .build();

        given(repo.findAllById(List.of(potatoId, garlicId, noUnitId)))
                .willReturn(List.of(potato, garlic, noUnit));
        given(ingredientUnitRepository.findAllByIngredientIdIn(List.of(potatoId, garlicId, noUnitId)))
                .willReturn(List.of(garlicUnit, potatoUnit));

        IngredientUnitsBatchResponse result = ingredientService.findUnitsByIngredientIds(
                List.of(potatoId, garlicId, potatoId, noUnitId));

        assertThat(result.getItems()).hasSize(3);
        assertThat(result.getItems().get(0).getIngredientId()).isEqualTo(potatoId);
        assertThat(result.getItems().get(0).getUnits()).extracting("unit").containsExactly("개");
        assertThat(result.getItems().get(1).getIngredientId()).isEqualTo(garlicId);
        assertThat(result.getItems().get(1).getUnits()).extracting("unit").containsExactly("쪽");
        assertThat(result.getItems().get(2).getIngredientId()).isEqualTo(noUnitId);
        assertThat(result.getItems().get(2).getUnits()).isEmpty();
    }

    @Test
    @DisplayName("findUnitsByIngredientIds: throws INGREDIENT_NOT_FOUND when any requested ingredient is missing")
    void findUnitsByIngredientIds_missingIngredient() {
        Long potatoId = 10L;
        Long missingId = 999L;
        Ingredient potato = Ingredient.builder().id(potatoId).name("potato").build();
        given(repo.findAllById(List.of(potatoId, missingId))).willReturn(List.of(potato));

        assertThatThrownBy(() -> ingredientService.findUnitsByIngredientIds(List.of(potatoId, missingId)))
                .isInstanceOf(CustomException.class)
                .extracting(e -> ((CustomException) e).getErrorCode())
                .isEqualTo(ErrorCode.INGREDIENT_NOT_FOUND);
        verifyNoInteractions(ingredientUnitRepository);
    }
}
