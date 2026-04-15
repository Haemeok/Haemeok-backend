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

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.*;

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
        given(ingredientRepository.findAll()).willReturn(List.of(master));

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

        // Then
        assertThat(totalCost).isEqualTo(0);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());

        RecipeIngredient savedEntity = captor.getValue();
        assertThat(savedEntity.getQuantity()).isEqualTo("약간");
        assertThat(savedEntity.getPrice().intValue()).isEqualTo(0);
        assertThat(savedEntity.getIngredient()).isNotNull();
        assertThat(savedEntity.getIngredient().getId()).isEqualTo(master.getId());
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

        given(ingredientRepository.findAll()).willReturn(List.of(master));

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

        // Then
        assertThat(totalCost).isEqualTo(2000);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());

        RecipeIngredient savedEntity = captor.getValue();
        assertThat(savedEntity.getRecipe().getId()).isEqualTo(dummyRecipe.getId());
        assertThat(savedEntity.getIngredient()).isNotNull();
        assertThat(savedEntity.getIngredient().getId()).isEqualTo(master.getId());
        assertThat(savedEntity.getQuantity()).isEqualTo("2");
        assertThat(savedEntity.getUnit()).isEqualTo("g");
        assertThat(savedEntity.getPrice().intValue()).isEqualTo(2000);
        assertThat(savedEntity.getCustomName()).isNull();
        assertThat(savedEntity.getCustomPrice()).isEqualTo(0); // 마스터 재료 사용 시 customPrice는 0으로 초기화
        assertThat(savedEntity.getCustomUnit()).isNull();
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
        given(ingredientRepository.findAll()).willReturn(List.of(master));

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

        // Then
        assertThat(totalCost).isEqualTo(1000);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());
        RecipeIngredient savedEntity = captor.getValue();
        assertThat(savedEntity.getQuantity()).isEqualTo("1/2");
        assertThat(savedEntity.getUnit()).isEqualTo("kg");
        assertThat(savedEntity.getPrice().intValue()).isEqualTo(1000);
    }

    @Test
    @DisplayName("saveAll: 마스터 재료가 없고, USER 모드에서 customPrice 혹은 customUnit이 없으면 예외 발생")
    void saveAll_masterIngredientMissing_userMode_throwException() {
        given(ingredientRepository.findAll()).willReturn(List.of());

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("새로운재료")
                .quantity("1")
                .customUnit(null)
                .customPrice(null)
                .build();

        // When & Then
        assertThatThrownBy(() -> recipeIngredientService.saveAll(dummyRecipe, List.of(dto), RecipeSourceType.USER))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.CUSTOM_INGREDIENT_INFO_MISSING));
    }

    @Test
    @DisplayName("saveAll: quantity가 숫자로 변환 불가한 형식일 때, '약간'으로 자동 보정되어 저장된다")
    void saveAll_invalidQuantity_correctedToSpecialWord() {
        given(ingredientRepository.findAll()).willReturn(List.of());

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("감자")
                .quantity("abc")
                .customUnit("g")
                .customPrice(BigDecimal.valueOf(1000))
                .build();

        // 서비스는 예외를 던지지 않고 수량을 "약간"으로 보정 후 저장
        recipeIngredientService.saveAll(dummyRecipe, List.of(dto), RecipeSourceType.USER);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());
        assertThat(captor.getValue().getQuantity()).isEqualTo("약간");
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

        // When & Then
        assertThatThrownBy(() -> recipeIngredientService.saveAll(dummyRecipe, List.of(dto), RecipeSourceType.USER))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.INVALID_INPUT_VALUE));
    }

    @Test
    @DisplayName("saveAll: AI 모드에서 마스터 재료가 없고 customUnit만 있을 경우, price=0으로 계산")
    void saveAll_masterMissingAiMode_priceZero() {
        given(ingredientRepository.findAll()).willReturn(List.of());

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
        // Then
        assertThat(totalCost).isEqualTo(0);

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);
        verify(recipeIngredientRepository, times(1)).save(captor.capture());
        RecipeIngredient savedEntity = captor.getValue();

        assertThat(savedEntity.getIngredient()).isNull();
        assertThat(savedEntity.getQuantity()).isEqualTo("3");
        assertThat(savedEntity.getUnit()).isEqualTo("개");
        assertThat(savedEntity.getPrice().intValue()).isEqualTo(0);
    }

    // ─── updateIngredients: 커스텀 재료 영양성분 보존 ──────────────────────────

    @Test
    @DisplayName("updateIngredients: 커스텀 재료 수정 시 DTO에 없는 영양성분이 기존 값으로 채워지는 테스트")
    void updateIngredients_preservesNutritionFromExistingCustomIngredient() {
        // Given
        RecipeIngredient existingCustom = RecipeIngredient.builder()
                .id(10L)
                .recipe(dummyRecipe)
                .customName("간장")
                .customCalorie(BigDecimal.valueOf(60))
                .customCarbohydrate(BigDecimal.valueOf(10))
                .customProtein(BigDecimal.valueOf(5))
                .customFat(BigDecimal.valueOf(0.5))
                .customSugar(BigDecimal.valueOf(2))
                .customSodium(BigDecimal.valueOf(800))
                .customPrice(500)
                .build();

        given(recipeIngredientRepository.findByRecipeId(dummyRecipe.getId()))
                .willReturn(List.of(existingCustom));
        given(ingredientRepository.findAll()).willReturn(List.of());

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("간장")
                .quantity("2")
                .customUnit("ml")
                // 영양성분 미전송 (null)
                .build();

        // When
        recipeIngredientService.updateIngredients(dummyRecipe, List.of(dto), RecipeSourceType.USER);

        // Then — 기존 영양성분이 DTO에 채워졌는지 확인
        assertThat(dto.getCustomCalories()).isEqualByComparingTo(BigDecimal.valueOf(60));
        assertThat(dto.getCustomCarbohydrate()).isEqualByComparingTo(BigDecimal.valueOf(10));
        assertThat(dto.getCustomProtein()).isEqualByComparingTo(BigDecimal.valueOf(5));
        assertThat(dto.getCustomFat()).isEqualByComparingTo(BigDecimal.valueOf(0.5));
        assertThat(dto.getCustomSugar()).isEqualByComparingTo(BigDecimal.valueOf(2));
        assertThat(dto.getCustomSodium()).isEqualByComparingTo(BigDecimal.valueOf(800));
        assertThat(dto.getCustomPrice()).isEqualByComparingTo(BigDecimal.valueOf(500));
    }

    @Test
    @DisplayName("updateIngredients: DTO에 이미 영양성분 값이 있으면 기존 값으로 덮어쓰지 않는 테스트")
    void updateIngredients_doesNotOverwriteDtoNutritionIfAlreadyPresent() {
        // Given
        RecipeIngredient existingCustom = RecipeIngredient.builder()
                .id(11L)
                .recipe(dummyRecipe)
                .customName("소금")
                .customCalorie(BigDecimal.ZERO)
                .customSodium(BigDecimal.valueOf(38000))
                .build();

        given(recipeIngredientRepository.findByRecipeId(dummyRecipe.getId()))
                .willReturn(List.of(existingCustom));
        given(ingredientRepository.findAll()).willReturn(List.of());

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("소금")
                .quantity("약간")
                .customUnit("꼬집")
                .customCalories(BigDecimal.valueOf(99))  // 이미 입력된 값
                .build();

        // When
        recipeIngredientService.updateIngredients(dummyRecipe, List.of(dto), RecipeSourceType.USER);

        // Then — DTO에 미리 있던 값(99)이 유지되어야 함
        assertThat(dto.getCustomCalories()).isEqualByComparingTo(BigDecimal.valueOf(99));
    }

    @Test
    @DisplayName("updateIngredients: 마스터 재료로 전환 시 커스텀 영양성분 보존 로직이 적용되지 않는 테스트")
    void updateIngredients_doesNotPreserveNutrition_whenIngredientBecomeMaster() {
        // Given — 기존에 커스텀으로 저장된 재료
        RecipeIngredient existingCustom = RecipeIngredient.builder()
                .id(12L)
                .recipe(dummyRecipe)
                .customName("계란")
                .customCalorie(BigDecimal.valueOf(155))
                .build();

        // 이번에는 마스터 재료가 DB에 존재함
        Ingredient masterEgg = Ingredient.builder()
                .id(1L)
                .name("계란")
                .unit("개")
                .price(300)
                .build();

        given(recipeIngredientRepository.findByRecipeId(dummyRecipe.getId()))
                .willReturn(List.of(existingCustom));
        given(ingredientRepository.findAll()).willReturn(List.of(masterEgg));

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("계란")
                .quantity("2")
                .build();

        ArgumentCaptor<RecipeIngredient> captor = ArgumentCaptor.forClass(RecipeIngredient.class);

        // When
        recipeIngredientService.updateIngredients(dummyRecipe, List.of(dto), RecipeSourceType.USER);

        // Then — 마스터 재료로 매핑되어 ingredient != null, customName == null
        verify(recipeIngredientRepository).save(captor.capture());
        assertThat(captor.getValue().getIngredient()).isNotNull();
        assertThat(captor.getValue().getCustomName()).isNull();
    }

    @Test
    @DisplayName("updateIngredients: 기존 커스텀 재료의 step 재료도 함께 삭제되는 테스트")
    void updateIngredients_deletesStepIngredientsBeforeResave() {
        // Given
        RecipeIngredient existing = RecipeIngredient.builder()
                .id(20L)
                .recipe(dummyRecipe)
                .customName("참기름")
                .customCalorie(BigDecimal.valueOf(884))
                .build();

        given(recipeIngredientRepository.findByRecipeId(dummyRecipe.getId()))
                .willReturn(List.of(existing));
        given(ingredientRepository.findAll()).willReturn(List.of());

        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("참기름")
                .quantity("약간")
                .customUnit("ml")
                .build();

        // When
        recipeIngredientService.updateIngredients(dummyRecipe, List.of(dto), RecipeSourceType.USER);

        // Then — step 재료 삭제 확인
        verify(recipeStepIngredientRepository).deleteByRecipeIngredientId(20L);
        verify(recipeIngredientRepository).deleteByRecipeId(dummyRecipe.getId());
        verify(recipeIngredientRepository).flush();
    }

    // ─── updateIngredients: 프론트가 0을 보내도 커스텀 값이 보존되는지 회귀 테스트 ──
    // 배경: 커스텀 재료는 YouTube/AI 추출로만 채워진다(유저 직접 입력 경로 없음).
    //       프론트 폼이 빈 값을 BigDecimal.ZERO로 직렬화해도 기존 DB 값이 0으로 초기화되면 안 된다.

    @Test
    @DisplayName("updateIngredients: 프론트가 customPrice/영양소를 모두 0(ZERO)으로 보내도 기존 DB 값이 보존된다")
    void updateIngredients_preservesExistingValues_whenDtoSendsZero() {
        // given - DB에 저장된 커스텀 재료 (AI/YouTube가 채운 상태)
        RecipeIngredient existingCustom = RecipeIngredient.builder()
                .id(30L)
                .recipe(dummyRecipe)
                .customName("특제소스")
                .customPrice(3500)
                .customCalorie(BigDecimal.valueOf(120))
                .customCarbohydrate(BigDecimal.valueOf(15))
                .customProtein(BigDecimal.valueOf(3))
                .customFat(BigDecimal.valueOf(5))
                .customSugar(BigDecimal.valueOf(8))
                .customSodium(BigDecimal.valueOf(450))
                .build();

        given(recipeIngredientRepository.findByRecipeId(dummyRecipe.getId()))
                .willReturn(List.of(existingCustom));
        given(ingredientRepository.findAll()).willReturn(List.of());

        // 프론트가 폼 기본값 0을 그대로 직렬화해서 보낸 payload를 시뮬레이션
        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("특제소스")
                .quantity("1")
                .customUnit("큰술")
                .customPrice(BigDecimal.ZERO)
                .customCalories(BigDecimal.ZERO)
                .customCarbohydrate(BigDecimal.ZERO)
                .customProtein(BigDecimal.ZERO)
                .customFat(BigDecimal.ZERO)
                .customSugar(BigDecimal.ZERO)
                .customSodium(BigDecimal.ZERO)
                .build();

        // when
        recipeIngredientService.updateIngredients(dummyRecipe, List.of(dto), RecipeSourceType.USER);

        // then - carry-over가 ZERO를 "의도 없음"으로 간주하고 기존 값으로 복구해야 한다
        assertThat(dto.getCustomPrice()).isEqualByComparingTo(BigDecimal.valueOf(3500));
        assertThat(dto.getCustomCalories()).isEqualByComparingTo(BigDecimal.valueOf(120));
        assertThat(dto.getCustomCarbohydrate()).isEqualByComparingTo(BigDecimal.valueOf(15));
        assertThat(dto.getCustomProtein()).isEqualByComparingTo(BigDecimal.valueOf(3));
        assertThat(dto.getCustomFat()).isEqualByComparingTo(BigDecimal.valueOf(5));
        assertThat(dto.getCustomSugar()).isEqualByComparingTo(BigDecimal.valueOf(8));
        assertThat(dto.getCustomSodium()).isEqualByComparingTo(BigDecimal.valueOf(450));
    }

    @Test
    @DisplayName("updateIngredients: null과 0을 섞어 보내도 각 필드별로 기존 값이 복구된다")
    void updateIngredients_preservesExistingValues_whenDtoMixesNullAndZero() {
        // given
        RecipeIngredient existingCustom = RecipeIngredient.builder()
                .id(31L)
                .recipe(dummyRecipe)
                .customName("참깨드레싱")
                .customPrice(2000)
                .customCalorie(BigDecimal.valueOf(90))
                .customCarbohydrate(BigDecimal.valueOf(7))
                .customProtein(BigDecimal.valueOf(2))
                .customFat(BigDecimal.valueOf(6))
                .customSugar(BigDecimal.valueOf(4))
                .customSodium(BigDecimal.valueOf(300))
                .build();

        given(recipeIngredientRepository.findByRecipeId(dummyRecipe.getId()))
                .willReturn(List.of(existingCustom));
        given(ingredientRepository.findAll()).willReturn(List.of());

        // 일부는 null, 일부는 ZERO - 둘 다 "의도 없음"으로 간주돼야 한다
        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("참깨드레싱")
                .quantity("2")
                .customUnit("큰술")
                .customPrice(null)
                .customCalories(BigDecimal.ZERO)
                .customCarbohydrate(null)
                .customProtein(BigDecimal.ZERO)
                .customFat(null)
                .customSugar(BigDecimal.ZERO)
                .customSodium(null)
                .build();

        // when
        recipeIngredientService.updateIngredients(dummyRecipe, List.of(dto), RecipeSourceType.USER);

        // then
        assertThat(dto.getCustomPrice()).isEqualByComparingTo(BigDecimal.valueOf(2000));
        assertThat(dto.getCustomCalories()).isEqualByComparingTo(BigDecimal.valueOf(90));
        assertThat(dto.getCustomCarbohydrate()).isEqualByComparingTo(BigDecimal.valueOf(7));
        assertThat(dto.getCustomProtein()).isEqualByComparingTo(BigDecimal.valueOf(2));
        assertThat(dto.getCustomFat()).isEqualByComparingTo(BigDecimal.valueOf(6));
        assertThat(dto.getCustomSugar()).isEqualByComparingTo(BigDecimal.valueOf(4));
        assertThat(dto.getCustomSodium()).isEqualByComparingTo(BigDecimal.valueOf(300));
    }

    @Test
    @DisplayName("updateIngredients: 프론트가 실제 non-zero 값을 echo하면 그대로 유지되고 DB 값으로 덮어쓰지 않는다")
    void updateIngredients_keepsDtoValues_whenNonZeroEchoed() {
        // given - 프론트가 응답 DTO의 per-unit 원본값을 그대로 round-trip 해준 경우
        RecipeIngredient existingCustom = RecipeIngredient.builder()
                .id(32L)
                .recipe(dummyRecipe)
                .customName("매운양념")
                .customPrice(1000)
                .customCalorie(BigDecimal.valueOf(50))
                .build();

        given(recipeIngredientRepository.findByRecipeId(dummyRecipe.getId()))
                .willReturn(List.of(existingCustom));
        given(ingredientRepository.findAll()).willReturn(List.of());

        // 프론트가 새로 내려준 응답값을 그대로 echo
        RecipeIngredientRequestDto dto = RecipeIngredientRequestDto.builder()
                .name("매운양념")
                .quantity("1")
                .customUnit("큰술")
                .customPrice(BigDecimal.valueOf(1000))
                .customCalories(BigDecimal.valueOf(50))
                .build();

        // when
        recipeIngredientService.updateIngredients(dummyRecipe, List.of(dto), RecipeSourceType.USER);

        // then - non-zero 값은 carry-over가 건드리지 않는다
        assertThat(dto.getCustomPrice()).isEqualByComparingTo(BigDecimal.valueOf(1000));
        assertThat(dto.getCustomCalories()).isEqualByComparingTo(BigDecimal.valueOf(50));
    }
}
