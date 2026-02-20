package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepIngredientRepository;
import com.jdc.recipe_service.domain.type.recipe.RecipeSourceType;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeIngredientServiceTest {

    @Mock
    private IngredientRepository ingredientRepository;

    @Mock
    private RecipeIngredientRepository recipeIngredientRepository;

    @Mock
    private RecipeStepIngredientRepository recipeStepIngredientRepository;

    @InjectMocks
    private RecipeIngredientService recipeIngredientService;

    private Recipe dummyRecipe;

    @BeforeEach
    void setUp() {
        dummyRecipe = Recipe.builder()
                .id(1L)
                .title("테스트 레시피")
                .description("테스트 설명")
                .build();
    }

    @Test
    @DisplayName("saveAll: 특별 수량 단어(약간) 입력 시, 총 가격은 0이며 DB에 원본 문자열이 저장된다")
    void saveAll_specialQuantityWord_priceIsZeroAndQuantityStringSaved() {
        Ingredient master = Ingredient.builder()
                .id(99L)
                .name("소금")
                .unit("g")
                .price(500)
                .build();
        when(ingredientRepository.findAll()).thenReturn(List.of(master));

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("소금")
                .quantity("약간")
                .customUnit(null)
                .customPrice(null)
                .build();

        int totalCost = recipeIngredientService.saveAll(
                dummyRecipe,
                List.of(dto),
                RecipeSourceType.USER
        );

        assertEquals(0, totalCost, "총 가격은 0이어야 합니다.");

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());

        RecipeIngredient savedEntity = captor.getValue();
        assertEquals("약간", savedEntity.getQuantity(), "DB에는 원본 수량 문자열이 저장되어야 합니다.");
        assertEquals(0, savedEntity.getPrice().intValue(), "계산된 가격은 0이어야 합니다.");
        assertNotNull(savedEntity.getIngredient());
        assertEquals(master.getId(), savedEntity.getIngredient().getId());
    }

    @Test
    @DisplayName("saveAll: 마스터 재료가 존재할 때, price(총 가격)가 올바르게 계산되고 저장된다")
    void saveAll_masterIngredientExists() {
        Ingredient master = Ingredient.builder()
                .id(99L)
                .name("감자")
                .unit("g")
                .price(1000)
                .build();

        when(ingredientRepository.findAll()).thenReturn(List.of(master));

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("감자")
                .quantity("2")
                .customUnit(null)
                .customPrice(null)
                .build();

        int totalCost = recipeIngredientService.saveAll(
                dummyRecipe,
                List.of(dto),
                RecipeSourceType.USER
        );

        assertEquals(2000, totalCost);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());

        RecipeIngredient savedEntity = captor.getValue();
        assertEquals(dummyRecipe.getId(), savedEntity.getRecipe().getId());
        assertNotNull(savedEntity.getIngredient());
        assertEquals(master.getId(), savedEntity.getIngredient().getId());
        assertEquals("2", savedEntity.getQuantity());
        assertEquals("g", savedEntity.getUnit());
        assertEquals(2000, savedEntity.getPrice().intValue());
        assertNull(savedEntity.getCustomName());
        assertNull(savedEntity.getCustomPrice());
        assertNull(savedEntity.getCustomUnit());
    }

    @Test
    @DisplayName("saveAll: 분수 형태(quantity=\"1/2\") 값 파싱 후 가격이 올바르게 계산된다")
    void saveAll_fractionQuantity_masterExists() {
        Ingredient master = Ingredient.builder()
                .id(101L)
                .name("밀가루")
                .unit("kg")
                .price(2000)
                .build();
        when(ingredientRepository.findAll()).thenReturn(List.of(master));

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("밀가루")
                .quantity("1/2")
                .customUnit(null)
                .customPrice(null)
                .build();

        int totalCost = recipeIngredientService.saveAll(
                dummyRecipe,
                List.of(dto),
                RecipeSourceType.USER
        );

        assertEquals(1000, totalCost);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());
        RecipeIngredient savedEntity = captor.getValue();
        assertEquals("1/2", savedEntity.getQuantity());
        assertEquals("kg", savedEntity.getUnit());
        assertEquals(1000, savedEntity.getPrice().intValue());
    }

    @Test
    @DisplayName("saveAll: 마스터 재료가 없고, USER 모드에서 customPrice 혹은 customUnit이 없으면 예외 발생")
    void saveAll_masterIngredientMissing_userMode_throwException() {
        when(ingredientRepository.findAll()).thenReturn(List.of());

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("새로운재료")
                .quantity("1")
                .customUnit(null)
                .customPrice(null)
                .build();

        CustomException ex = assertThrows(CustomException.class, () -> {
            recipeIngredientService.saveAll(
                    dummyRecipe,
                    List.of(dto),
                    RecipeSourceType.USER
            );
        });

        assertEquals(ErrorCode.CUSTOM_INGREDIENT_INFO_MISSING, ex.getErrorCode());
    }

    @Test
    @DisplayName("saveAll: quantity가 잘못된 형식일 때, INVALID_INGREDIENT_QUANTITY 예외 발생")
    void saveAll_invalidQuantity_throwException() {
        when(ingredientRepository.findAll()).thenReturn(List.of());

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("감자")
                .quantity("abc")
                .customUnit("g")
                .customPrice(BigDecimal.valueOf(1000))
                .build();

        CustomException ex = assertThrows(CustomException.class, () -> {
            recipeIngredientService.saveAll(
                    dummyRecipe,
                    List.of(dto),
                    RecipeSourceType.USER
            );
        });

        assertEquals(ErrorCode.INVALID_INGREDIENT_QUANTITY, ex.getErrorCode());
    }

    @Test
    @DisplayName("saveAll: name이 빈 문자열일 때, INVALID_INPUT_VALUE 예외 발생")
    void saveAll_blankName_throwException() {
        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("  ")
                .quantity("1")
                .customUnit("g")
                .customPrice(BigDecimal.valueOf(1000))
                .build();

        CustomException ex = assertThrows(CustomException.class, () -> {
            recipeIngredientService.saveAll(
                    dummyRecipe,
                    List.of(dto),
                    RecipeSourceType.USER
            );
        });

        assertEquals(ErrorCode.INVALID_INPUT_VALUE, ex.getErrorCode());
    }

    @Test
    @DisplayName("saveAll: AI 모드에서 마스터 재료가 없고 customUnit만 있을 경우, price=0으로 계산")
    void saveAll_masterMissingAiMode_priceZero() {
        when(ingredientRepository.findAll()).thenReturn(List.of());

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("AI재료")
                .quantity("3")
                .customUnit("개")
                .customPrice(null)
                .build();

        int totalCost = recipeIngredientService.saveAll(
                dummyRecipe,
                List.of(dto),
                RecipeSourceType.AI
        );
        assertEquals(0, totalCost);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());
        RecipeIngredient savedEntity = captor.getValue();

        assertNull(savedEntity.getIngredient());
        assertEquals("3", savedEntity.getQuantity());
        assertEquals("개", savedEntity.getUnit());
        assertEquals(0, savedEntity.getPrice().intValue());
    }
}
