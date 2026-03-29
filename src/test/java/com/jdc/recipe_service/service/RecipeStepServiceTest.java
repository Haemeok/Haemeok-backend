package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.entity.RecipeStepIngredient;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.util.ActionImageService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeStepServiceTest {

    @Mock
    private RecipeIngredientRepository recipeIngredientRepository;

    @Mock
    private RecipeStepRepository recipeStepRepository;

    @Mock
    private RecipeStepIngredientRepository recipeStepIngredientRepository;

    @Mock
    private ActionImageService actionImageService;

    @InjectMocks
    private RecipeStepService service;

    private Recipe recipe;
    private RecipeIngredient ingr1;
    private RecipeIngredient ingr2;

    @BeforeEach
    void setUp() {
        // 기본 Recipe 엔티티 생성
        recipe = Recipe.builder()
                .id(10L)
                .build();

        // 레시피 재료 샘플 생성
        // 여기서는 ingredient 필드를 사용하지 않고, customName만 채운다.
        ingr1 = RecipeIngredient.builder()
                .id(100L)
                .customName("감자")
                .quantity("2")
                .unit("개")
                .build();

        ingr2 = RecipeIngredient.builder()
                .id(101L)
                .customName("양파")
                .quantity("1")
                .unit("개")
                .build();
    }

    @Test
    @DisplayName("getStepsByRecipeId: 레포에서 가져온 리스트를 그대로 반환하는 테스트")
    void getStepsByRecipeId_success() {
        // Given
        RecipeStep step1 = RecipeStep.builder()
                .id(1L)
                .stepNumber(1)
                .instruction("씻기")
                .recipe(recipe)
                .build();
        RecipeStep step2 = RecipeStep.builder()
                .id(2L)
                .stepNumber(2)
                .instruction("썰기")
                .recipe(recipe)
                .build();

        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(10L))
                .willReturn(List.of(step1, step2));

        // When
        var actual = service.getStepsByRecipeId(10L);

        // Then
        assertThat(actual).hasSize(2);
        assertThat(actual.get(0)).isEqualTo(step1);
        assertThat(actual.get(1)).isEqualTo(step2);
        verify(recipeStepRepository, times(1)).findByRecipeIdOrderByStepNumber(10L);
    }

    @Test
    @DisplayName("saveAll: 새로운 단계와 단계별 재료를 모두 저장하는 테스트")
    void saveAll_success() {
        // Given
        given(recipeIngredientRepository.findByRecipeId(10L))
                .willReturn(List.of(ingr1, ingr2));

        // DTO: stepNumber, instruction, imageKey, action, ingredients 리스트
        RecipeStepIngredientRequestDto siDto1 = new RecipeStepIngredientRequestDto();
        siDto1.setName("감자");
        siDto1.setQuantity("2");
        siDto1.setCustomUnit("개");

        RecipeStepRequestDto dto1 = new RecipeStepRequestDto();
        dto1.setStepNumber(1);
        dto1.setInstruction("감자 씻기");
        dto1.setImageKey("key1");
        dto1.setAction("씻기");
        dto1.setIngredients(List.of(siDto1));

        // When
        service.saveAll(recipe, List.of(dto1));

        // Then
        // 1) RecipeStep 저장 검증
        ArgumentCaptor<RecipeStep> stepCaptor = ArgumentCaptor.forClass(RecipeStep.class);
        verify(recipeStepRepository, times(1)).save(stepCaptor.capture());
        RecipeStep savedStep = stepCaptor.getValue();
        assertThat(savedStep.getStepNumber()).isEqualTo(1);
        assertThat(savedStep.getInstruction()).isEqualTo("감자 씻기");
        assertThat(savedStep.getImageKey()).isEqualTo("key1");
        assertThat(savedStep.getAction()).isEqualTo("씻기");
        assertThat(savedStep.getRecipe()).isEqualTo(recipe);

        // 2) 단계별 재료 저장 검증
        ArgumentCaptor<RecipeStepIngredient> stepIngrCaptor = ArgumentCaptor.forClass(RecipeStepIngredient.class);
        verify(recipeStepIngredientRepository, times(1)).save(stepIngrCaptor.capture());
        RecipeStepIngredient savedStepIngr = stepIngrCaptor.getValue();

        // 이제 ingredient 필드가 null이므로, 대신 customName("감자")이 설정되었는지 확인한다.
        assertThat(savedStepIngr.getIngredient()).isNull();
        assertThat(savedStepIngr.getCustomName()).isEqualTo("감자");
        assertThat(savedStepIngr.getQuantity()).isEqualTo("2");
        assertThat(savedStepIngr.getUnit()).isEqualTo("개");
        assertThat(savedStepIngr.getStep()).isEqualTo(savedStep);
    }

    @Test
    @DisplayName("saveAll: 존재하지 않는 재료명을 넘기면 INGREDIENT_NOT_FOUND 예외 발생하는 테스트 (단계 저장 후 재료 저장 전 예외)")
    void saveAll_missingIngredient_throw() {
        // Given
        given(recipeIngredientRepository.findByRecipeId(10L))
                .willReturn(List.of(ingr1)); // "양파"는 맵에 없음

        RecipeStepIngredientRequestDto siDto = new RecipeStepIngredientRequestDto();
        siDto.setName("양파");
        siDto.setQuantity("1");
        siDto.setCustomUnit("개");

        RecipeStepRequestDto dto = new RecipeStepRequestDto();
        dto.setStepNumber(1);
        dto.setInstruction("감자 씻기");
        dto.setImageKey("k");
        dto.setAction("씻기");
        dto.setIngredients(List.of(siDto));

        // When & Then
        assertThatThrownBy(() -> service.saveAll(recipe, List.of(dto)))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> {
                    assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.INGREDIENT_NOT_FOUND);
                    assertThat(e.getMessage()).contains("양파");
                });

        // RecipeStep은 1회 저장되지만, RecipeStepIngredient는 저장되지 않아야 한다
        verify(recipeStepRepository, times(1)).save(any());
        verify(recipeStepIngredientRepository, never()).save(any());
    }

    @Test
    @DisplayName("updateSteps: 없어진 step은 삭제, 기존 step은 업데이트, 새 step은 추가하는 테스트")
    void updateSteps_success() {
        // Given
        given(recipeIngredientRepository.findByRecipeId(10L))
                .willReturn(List.of(ingr1, ingr2));

        // 기존에 DB에 저장된 단계 1,2 준비
        RecipeStep existing1 = RecipeStep.builder()
                .id(1L)
                .stepNumber(1)
                .instruction("기존1")
                .imageKey("oldKey1")
                .action("삭기")
                .recipe(recipe)
                .build();
        RecipeStep existing2 = RecipeStep.builder()
                .id(2L)
                .stepNumber(2)
                .instruction("기존2")
                .imageKey("oldKey2")
                .action("썰기")
                .recipe(recipe)
                .build();

        // step2에 기존 재료 1개 달려 있다고 가정 (customName 기준)
        RecipeStepIngredient existingStepIngr = RecipeStepIngredient.builder()
                .id(50L)
                .step(existing2)
                .customName("양파")
                .quantity("1")
                .unit("개")
                .build();
        existing2.getStepIngredients().add(existingStepIngr);

        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(10L))
                .willReturn(List.of(existing1, existing2));

        // --- 1번 단계 수정 DTO 준비 ---
        RecipeStepIngredientRequestDto newSi1 = new RecipeStepIngredientRequestDto();
        newSi1.setName("감자");
        newSi1.setQuantity("3");
        newSi1.setCustomUnit("개");

        RecipeStepRequestDto dto1 = new RecipeStepRequestDto();
        dto1.setStepNumber(1);
        dto1.setInstruction("수정된1");
        dto1.setImageKey("newKey1");
        dto1.setAction("섞기");
        dto1.setIngredients(List.of(newSi1));

        // --- 3번 단계 새로 추가 DTO 준비 ---
        RecipeStepIngredientRequestDto si3 = new RecipeStepIngredientRequestDto();
        si3.setName("감자");
        si3.setQuantity("1");
        si3.setCustomUnit("개");

        RecipeStepRequestDto dto3 = new RecipeStepRequestDto();
        dto3.setStepNumber(3);
        dto3.setInstruction("새로운3");
        dto3.setImageKey("key3");
        dto3.setAction("끓이기");
        dto3.setIngredients(List.of(si3));

        // When
        service.updateSteps(recipe, List.of(dto1, dto3));

        // Then
        // 1) 2번 단계는 삭제되어야 한다
        verify(recipeStepRepository, times(1)).delete(existing2);

        // 2) 1번 단계 필드가 업데이트되었는지 확인
        assertThat(existing1.getInstruction()).isEqualTo("수정된1");
        assertThat(existing1.getImageKey()).isEqualTo("newKey1");
        assertThat(existing1.getAction()).isEqualTo("섞기");

        // 3) existing1 단계에 재료(감자)가 새로 추가되었는지 확인
        ArgumentCaptor<RecipeStepIngredient> capExist1 = ArgumentCaptor.forClass(RecipeStepIngredient.class);
        verify(recipeStepIngredientRepository, atLeast(1)).save(capExist1.capture());
        List<RecipeStepIngredient> savedList = capExist1.getAllValues();
        boolean foundForExist1 = savedList.stream()
                .anyMatch(rsi ->
                        rsi.getStep().equals(existing1)
                                && Objects.equals(rsi.getCustomName(), "감자")
                );
        assertThat(foundForExist1).as("1번 단계에 customName = '감자'이 추가되어야 한다").isTrue();

        // 4) 3번 단계가 새로 생성되고 save()가 호출되었는지 확인
        ArgumentCaptor<RecipeStep> newStepCap = ArgumentCaptor.forClass(RecipeStep.class);
        verify(recipeStepRepository, times(1)).save(newStepCap.capture());
        RecipeStep savedStep3 = newStepCap.getValue();
        assertThat(savedStep3.getStepNumber()).isEqualTo(3);
        assertThat(savedStep3.getInstruction()).isEqualTo("새로운3");
        assertThat(savedStep3.getImageKey()).isEqualTo("key3");
        assertThat(savedStep3.getAction()).isEqualTo("끓이기");
        assertThat(savedStep3.getRecipe()).isEqualTo(recipe);

        // 5) 3번 단계의 재료도 save()가 최소 1회 호출되었는지 확인 (customName="감자")
        boolean foundForStep3 = savedList.stream()
                .anyMatch(rsi ->
                        Objects.equals(rsi.getStep(), savedStep3)
                                && Objects.equals(rsi.getCustomName(), "감자")
                );
        assertThat(foundForStep3).as("3번 단계에 customName = '감자'이 추가되어야 한다").isTrue();

        // 전체적으로 RecipeStepIngredientRepository.save()는 최소 2회 호출
        verify(recipeStepIngredientRepository, atLeast(2)).save(any());
    }

    @Test
    @DisplayName("updateSteps: 잘못된 재료가 DTO에 있으면 INGREDIENT_NOT_FOUND 예외 발생하는 테스트")
    void updateSteps_missingIngredient_throw() {
        // Given
        given(recipeIngredientRepository.findByRecipeId(10L))
                .willReturn(List.of(ingr1));

        // 기존 단계 하나, id=1
        RecipeStep existing = RecipeStep.builder()
                .id(1L)
                .stepNumber(1)
                .recipe(recipe)
                .build();
        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(10L))
                .willReturn(List.of(existing));

        // DTO에 존재하지 않는 "고추장"을 참조
        RecipeStepIngredientRequestDto wrongSi = new RecipeStepIngredientRequestDto();
        wrongSi.setName("고추장");
        wrongSi.setQuantity("1");
        wrongSi.setCustomUnit("스푼");

        RecipeStepRequestDto dto = new RecipeStepRequestDto();
        dto.setStepNumber(1);
        dto.setInstruction("수정");
        dto.setImageKey("k");
        dto.setAction("넣기");
        dto.setIngredients(List.of(wrongSi));

        // When & Then
        assertThatThrownBy(() -> service.updateSteps(recipe, List.of(dto)))
                .isInstanceOf(CustomException.class)
                .satisfies(e -> {
                    assertThat(((CustomException) e).getErrorCode()).isEqualTo(ErrorCode.INGREDIENT_NOT_FOUND);
                    assertThat(e.getMessage()).contains("고추장");
                });
    }

    @Test
    @DisplayName("deleteAllByRecipeId: 모든 단계와 연관된 단계별 재료를 벌크 삭제하는 테스트")
    void deleteAllByRecipeId_success() {
        // Given
        Long recipeId = 10L;

        // When
        service.deleteAllByRecipeId(recipeId);

        // Then
        verify(recipeStepIngredientRepository, times(1)).deleteAllByRecipeId(recipeId);

        verify(recipeStepRepository, times(1)).deleteByRecipeId(recipeId);

        verify(recipeStepIngredientRepository, never()).deleteByStepId(anyLong());
    }

    @Test
    @DisplayName("updateStepsFromUser: action 없이 instruction, imageKey만 업데이트하는 테스트")
    void updateStepsFromUser_success() {
        // Given
        given(recipeIngredientRepository.findByRecipeId(10L))
                .willReturn(List.of(ingr1));

        RecipeStep existing = RecipeStep.builder()
                .id(1L)
                .stepNumber(1)
                .instruction("기존")
                .imageKey("oldKey")
                .recipe(recipe)
                .build();

        given(recipeStepRepository.findByRecipeIdOrderByStepNumber(10L))
                .willReturn(List.of(existing));

        // DTO: action 없이 instruction과 imageKey만 반영
        RecipeStepIngredientRequestDto siDto = new RecipeStepIngredientRequestDto();
        siDto.setName("감자");
        siDto.setQuantity("5");
        siDto.setCustomUnit("개");

        RecipeStepRequestDto dto = new RecipeStepRequestDto();
        dto.setStepNumber(1);
        dto.setInstruction("새로운");
        dto.setImageKey("newKey");
        dto.setIngredients(List.of(siDto));

        // When
        service.updateStepsFromUser(recipe, List.of(dto));

        // Then
        // 기존 단계 instruction, imageKey만 바뀌고 action은 그대로 null
        assertThat(existing.getInstruction()).isEqualTo("새로운");
        assertThat(existing.getImageKey()).isEqualTo("newKey");
        assertThat(existing.getAction()).isNull();
    }
}
