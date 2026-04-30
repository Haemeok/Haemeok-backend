package com.jdc.recipe_service.dev.service.ingredient;

import com.jdc.recipe_service.dev.repository.recipe.DevTopRecipeQueryRepository;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientDetailDto;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientNutritionPer100gDto;
import com.jdc.recipe_service.domain.dto.ingredient.IngredientPairItemDto;
import com.jdc.recipe_service.domain.dto.recipe.RecipeSimpleDto;
import com.jdc.recipe_service.service.IngredientService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
class DevIngredientServiceTest {

    @Mock IngredientService ingredientService;
    @Mock DevTopRecipeQueryRepository devTopRecipeQueryRepository;

    private DevIngredientService devService;

    private static final Long INGREDIENT_ID = 42L;

    @BeforeEach
    void setUp() {
        devService = new DevIngredientService(ingredientService, devTopRecipeQueryRepository);
    }

    @Test
    @DisplayName("findDetailByIdDev: preserves all base fields and replaces only recipes with dev policy results")
    void findDetailByIdDev_replacesOnlyRecipes_keepsAllOtherFields() {
        IngredientDetailDto base = IngredientDetailDto.builder()
                .id(INGREDIENT_ID)
                .name("potato")
                .category("vegetable")
                .imageUrl("https://example.com/image.webp")
                .coupangLink("https://link.coupang.com/a/potato")
                .nutritionPer100g(IngredientNutritionPer100gDto.builder()
                        .kcal(new BigDecimal("77.000"))
                        .carbohydrateG(new BigDecimal("17.500"))
                        .build())
                .storageLocation("fridge")
                .storageTemperature("0~4C")
                .storageDuration("1~2 weeks")
                .storageNotes("keep dry")
                .goodPairs("Garlic")
                .goodPairItems(List.of(IngredientPairItemDto.builder()
                        .id(10L)
                        .name("Garlic")
                        .imageUrl("https://example.com/garlic.webp")
                        .build()))
                .badPairs("Milk")
                .badPairItems(List.of(IngredientPairItemDto.builder()
                        .id(11L)
                        .name("Milk")
                        .imageUrl("https://example.com/milk.webp")
                        .build()))
                .benefits("Rich in vitamin C")
                .seasonMonths(List.of(6, 7, 8))
                .recommendedCookingMethods("roast / fry")
                .recipes(List.of(RecipeSimpleDto.builder().id(1L).title("operational recipe").build()))
                .build();
        given(ingredientService.findDetailById(INGREDIENT_ID)).willReturn(base);

        List<RecipeSimpleDto> devRecipes = List.of(
                RecipeSimpleDto.builder().id(101L).title("dev pub 1").build(),
                RecipeSimpleDto.builder().id(102L).title("dev pub 2").build()
        );
        given(devTopRecipeQueryRepository.findTopByIngredientIdDev(eq(INGREDIENT_ID), eq(10)))
                .willReturn(devRecipes);

        IngredientDetailDto result = devService.findDetailByIdDev(INGREDIENT_ID);

        assertThat(result.getId()).isEqualTo(INGREDIENT_ID);
        assertThat(result.getName()).isEqualTo("potato");
        assertThat(result.getCategory()).isEqualTo("vegetable");
        assertThat(result.getImageUrl()).isEqualTo("https://example.com/image.webp");
        assertThat(result.getCoupangLink()).isEqualTo("https://link.coupang.com/a/potato");
        assertThat(result.getNutritionPer100g().getKcal()).isEqualByComparingTo("77.000");
        assertThat(result.getStorageLocation()).isEqualTo("fridge");
        assertThat(result.getGoodPairs()).isEqualTo("Garlic");
        assertThat(result.getGoodPairItems()).extracting(IngredientPairItemDto::getId).containsExactly(10L);
        assertThat(result.getBadPairs()).isEqualTo("Milk");
        assertThat(result.getBadPairItems()).extracting(IngredientPairItemDto::getId).containsExactly(11L);
        assertThat(result.getBenefits()).isEqualTo("Rich in vitamin C");
        assertThat(result.getSeasonMonths()).containsExactly(6, 7, 8);
        assertThat(result.getRecommendedCookingMethods()).isEqualTo("roast / fry");
        assertThat(result.getRecipes()).extracting(RecipeSimpleDto::getId).containsExactly(101L, 102L);
    }

    @Test
    @DisplayName("findDetailByIdDev: passes top recipe limit=10")
    void findDetailByIdDev_passesLimit10() {
        IngredientDetailDto base = IngredientDetailDto.builder()
                .id(INGREDIENT_ID).name("potato").build();
        given(ingredientService.findDetailById(INGREDIENT_ID)).willReturn(base);
        given(devTopRecipeQueryRepository.findTopByIngredientIdDev(eq(INGREDIENT_ID), eq(10)))
                .willReturn(List.of());

        devService.findDetailByIdDev(INGREDIENT_ID);

        verify(devTopRecipeQueryRepository).findTopByIngredientIdDev(INGREDIENT_ID, 10);
    }

    @Test
    @DisplayName("findDetailByIdDev: returns empty dev recipes")
    void findDetailByIdDev_emptyDevRecipes_returnsEmptyList() {
        IngredientDetailDto base = IngredientDetailDto.builder()
                .id(INGREDIENT_ID).name("potato").storageLocation("fridge").build();
        given(ingredientService.findDetailById(INGREDIENT_ID)).willReturn(base);
        given(devTopRecipeQueryRepository.findTopByIngredientIdDev(eq(INGREDIENT_ID), eq(10)))
                .willReturn(List.of());

        IngredientDetailDto result = devService.findDetailByIdDev(INGREDIENT_ID);

        assertThat(result.getRecipes()).isEmpty();
        assertThat(result.getStorageLocation()).isEqualTo("fridge");
    }
}
