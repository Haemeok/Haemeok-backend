package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeTag;
import com.jdc.recipe_service.domain.repository.RecipeTagRepository;
import com.jdc.recipe_service.domain.type.TagType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.*;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.*;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.BDDMockito.*;

@ExtendWith(MockitoExtension.class)
class RecipeTagServiceTest {

    @Mock
    private RecipeTagRepository recipeTagRepository;

    @InjectMocks
    private RecipeTagService recipeTagService;

    private Recipe dummyRecipe;

    @BeforeEach
    void setUp() {
        dummyRecipe = Recipe.builder()
                .id(10L)
                .title("테스트 레시피 태그")
                .build();
    }

    @Test
    @DisplayName("saveAll: 중복 태그가 있어도 한 번만 저장된다")
    void saveAll_distinctTags() {
        // Given
        var inputTags = List.of("🍽️ 혼밥", "🍽️ 혼밥", "⚡ 초스피드 / 간단 요리");

        given(recipeTagRepository.saveAll(anyList()))
                .willReturn(Collections.emptyList());

        // When
        recipeTagService.saveAll(dummyRecipe, inputTags);

        // Then
        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<RecipeTag>> captor = ArgumentCaptor.forClass(List.class);
        verify(recipeTagRepository, times(1)).saveAll(captor.capture());

        List<RecipeTag> savedList = captor.getValue();
        Set<TagType> savedTypes = new HashSet<>();
        for (var rt : savedList) {
            assertThat(rt.getRecipe().getId()).isEqualTo(dummyRecipe.getId());
            savedTypes.add(rt.getTag());
        }
        assertThat(savedTypes).contains(TagType.SOLO);
        assertThat(savedTypes).contains(TagType.QUICK);
        assertThat(savedTypes).hasSize(2);
    }

    @Test
    @DisplayName("saveAll: 빈 리스트 입력 시 저장을 하지 않는다")
    void saveAll_emptyInput_noSave() {
        // Given
        var inputTags = Collections.<String>emptyList();

        // When
        recipeTagService.saveAll(dummyRecipe, inputTags);

        // Then
        // 빈 리스트인 경우 saveAll 호출이 없어야 한다
        verify(recipeTagRepository, never()).saveAll(anyList());
    }

    @Test
    @DisplayName("updateTags: 기존 태그가 있고, 새로운 태그 목록으로 업데이트된다")
    void updateTags_addAndRemoveTags() {
        // Given
        // 기존 태그: SOLO, PICNIC
        var existingSolo = RecipeTag.builder()
                .id(100L)
                .recipe(dummyRecipe)
                .tag(TagType.SOLO)
                .build();
        var existingPicnic = RecipeTag.builder()
                .id(101L)
                .recipe(dummyRecipe)
                .tag(TagType.PICNIC)
                .build();
        var existingList = List.of(existingSolo, existingPicnic);

        // 새 입력: SOLO(유지), HEALTHY(추가)
        var newTagDisplay = List.of("🍽️ 혼밥", "🥗 다이어트 / 건강식");

        given(recipeTagRepository.findByRecipeId(dummyRecipe.getId()))
                .willReturn(existingList);
        willDoNothing().given(recipeTagRepository).deleteAll(anyList());
        given(recipeTagRepository.saveAll(anyList()))
                .willReturn(Collections.emptyList());

        // When
        recipeTagService.updateTags(dummyRecipe, newTagDisplay);

        // Then
        // 1) PICNIC 은 삭제 대상
        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<RecipeTag>> removeCaptor = ArgumentCaptor.forClass(List.class);
        verify(recipeTagRepository, times(1)).deleteAll(removeCaptor.capture());
        var toRemove = removeCaptor.getValue();
        assertThat(toRemove).hasSize(1);
        assertThat(toRemove.get(0).getTag()).isEqualTo(TagType.PICNIC);

        // 2) HEALTHY 는 saveAll 로 한 번만 추가
        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<RecipeTag>> addAllCaptor = ArgumentCaptor.forClass(List.class);
        verify(recipeTagRepository, times(1)).saveAll(addAllCaptor.capture());
        var toAdd = addAllCaptor.getValue();
        assertThat(toAdd).hasSize(1);
        assertThat(toAdd.get(0).getTag()).isEqualTo(TagType.HEALTHY);
    }

    @Test
    @DisplayName("deleteAllByRecipeId: repository.deleteByRecipeId 메서드가 호출된다")
    void deleteAllByRecipeId_callsRepository() {
        // Given
        willDoNothing().given(recipeTagRepository).deleteByRecipeId(dummyRecipe.getId());

        // When
        recipeTagService.deleteAllByRecipeId(dummyRecipe.getId());

        // Then
        verify(recipeTagRepository, times(1)).deleteByRecipeId(dummyRecipe.getId());
    }
}
