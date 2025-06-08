package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepIngredientRepository;
import com.jdc.recipe_service.domain.type.RecipeSourceType;
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
        // 간단한 Recipe 엔티티 생성 (ID만 있어도 무방)
        dummyRecipe = Recipe.builder()
                .id(1L)
                .title("테스트 레시피")
                .description("테스트 설명")
                .build();
    }

    @Test
    @DisplayName("saveAll: 마스터 재료가 존재할 때, price(총 가격)가 올바르게 계산되고 저장된다")
    void saveAll_masterIngredientExists() {
        // 1) 마스터 재료를 하나 만들어서, findAll에서 반환하도록 모킹
        Ingredient master = Ingredient.builder()
                .id(99L)
                .name("감자")
                .unit("g")
                .price(1000)
                .build();

        when(ingredientRepository.findAll()).thenReturn(List.of(master));

        // 2) RecipeIngredientRequestDto 설정
        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("감자")         // toLowerCase로 비교하므로 대소문자 구분 없음
                .quantity("2")        // 2 (숫자 문자열)
                .customUnit(null)     // customUnit 없으므로 master.getUnit()인 "g" 사용
                .customPrice(null)    // master가 있으므로 customPrice 무시
                .build();

        // 3) service 호출
        int totalCost = recipeIngredientService.saveAll(
                dummyRecipe,
                List.of(dto),
                RecipeSourceType.USER // USER 모드
        );

        // 4) 1000원 * 2 = 2000원 이어야 함
        assertEquals(2000, totalCost);

        // 5) 저장되는 엔티티를 캡처하여 내부 필드를 검증
        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());

        RecipeIngredient savedEntity = captor.getValue();
        // 연관된 Recipe ID 확인
        assertEquals(dummyRecipe.getId(), savedEntity.getRecipe().getId());
        // 연관된 Ingredient ID 확인 (master ingredient ID: 99L)
        assertNotNull(savedEntity.getIngredient());
        assertEquals(master.getId(), savedEntity.getIngredient().getId());
        // quantity도 "2" 그대로 저장
        assertEquals("2", savedEntity.getQuantity());
        // 단위는 master.getUnit()인 "g"가 사용됨
        assertEquals("g", savedEntity.getUnit());
        // price(총 가격) 필드가 correct: 2 * 1000 = 2000
        assertEquals(2000, savedEntity.getPrice().intValue());
        // customName/customPrice/customUnit 모두 null (마스터 재료가 있었으므로)
        assertNull(savedEntity.getCustomName());
        assertNull(savedEntity.getCustomPrice());
        assertNull(savedEntity.getCustomUnit());
    }

    @Test
    @DisplayName("saveAll: 분수 형태(quantity=\"1/2\") 값 파싱 후 가격이 올바르게 계산된다")
    void saveAll_fractionQuantity_masterExists() {
        // 마스터 재료 준비
        Ingredient master = Ingredient.builder()
                .id(101L)
                .name("밀가루")
                .unit("kg")
                .price(2000)  // 1kg당 2000원
                .build();
        when(ingredientRepository.findAll()).thenReturn(List.of(master));

        // 분수 형태 수량
        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("밀가루")
                .quantity("1/2")  // 0.5kg
                .customUnit(null)
                .customPrice(null)
                .build();

        int totalCost = recipeIngredientService.saveAll(
                dummyRecipe,
                List.of(dto),
                RecipeSourceType.USER
        );

        // 2000원 * 0.5 = 1000원
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
        // 모든 마스터 재료가 없도록 빈 리스트 반환
        when(ingredientRepository.findAll()).thenReturn(List.of());

        // DTO에는 customUnit과 customPrice가 모두 null → 예외 발생
        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("새로운재료")
                .quantity("1")
                .customUnit(null)       // 단위 누락
                .customPrice(null)      // 가격 누락
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
                .quantity("abc")         // 숫자로 변환 불가
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
                .name("  ")               // 공백으로만 이루어진 이름
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
                .customUnit("개")         // 단위만 제공
                .customPrice(null)       // AI 모드이므로 customPrice 없어도 OK
                .build();

        // AI 모드에서는 customPrice가 없어도 예외 아니며, price=0이 되어야 함
        int totalCost = recipeIngredientService.saveAll(
                dummyRecipe,
                List.of(dto),
                RecipeSourceType.AI
        );
        assertEquals(0, totalCost);

        // 저장 시 총 price 필드가 0, 단위(unit)는 customUnit("개"), ingredient는 null
        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());
        RecipeIngredient savedEntity = captor.getValue();

        assertNull(savedEntity.getIngredient());       // 마스터 재료가 없으므로 null
        assertEquals("3", savedEntity.getQuantity());  // quantity 문자열은 그대로
        assertEquals("개", savedEntity.getUnit());     // customUnit 그대로 사용
        assertEquals(0, savedEntity.getPrice().intValue());  // price=0
        // AI 모드이므로 customName은 dto.getName()인 "AI재료"를 설정했어야 하나,
        // 실제 Mapper 구현에 따라 customName 세팅 여부가 다릅니다. 이 프로젝트 기준으로 검사하지 않음.
    }
}
