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

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

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
        // 1) 동일한 displayName 두 번 전달 (‘🍽️ 혼밥’)
        List<String> inputTags = List.of("🍽️ 혼밥", "🍽️ 혼밥", "⚡ 초스피드 / 간단 요리");

        // 2) saveAll은 List<RecipeTag> 반환이므로, 빈 리스트를 반환하도록 모킹
        when(recipeTagRepository.saveAll(anyList()))
                .thenReturn(Collections.emptyList());

        // 3) 실제 호출
        recipeTagService.saveAll(dummyRecipe, inputTags);

        // 4) 저장해야 할 실제 TagType은 두 가지: SOLO, QUICK
        ArgumentCaptor<List<RecipeTag>> captor = ArgumentCaptor.forClass(List.class);
        verify(recipeTagRepository, times(1)).saveAll(captor.capture());

        List<RecipeTag> savedList = captor.getValue();
        Set<TagType> savedTypes = new HashSet<>();
        for (RecipeTag rt : savedList) {
            assertEquals(dummyRecipe.getId(), rt.getRecipe().getId());
            savedTypes.add(rt.getTag());
        }
        assertTrue(savedTypes.contains(TagType.SOLO));
        assertTrue(savedTypes.contains(TagType.QUICK));
        assertEquals(2, savedTypes.size());
    }

    @Test
    @DisplayName("saveAll: 빈 리스트 입력 시에도 saveAll 한 번 호출")
    void saveAll_emptyInput_callsSaveAllOnce() {
        List<String> inputTags = Collections.emptyList();

        when(recipeTagRepository.saveAll(anyList()))
                .thenReturn(Collections.emptyList());

        recipeTagService.saveAll(dummyRecipe, inputTags);

        // 빈 리스트라도 한 번 호출되도록 기대
        ArgumentCaptor<List<RecipeTag>> captor = ArgumentCaptor.forClass(List.class);
        verify(recipeTagRepository, times(1)).saveAll(captor.capture());

        List<RecipeTag> savedList = captor.getValue();
        assertTrue(savedList.isEmpty());
    }

    @Test
    @DisplayName("updateTags: 기존 태그가 있고, 새로운 태그 목록으로 업데이트된다")
    void updateTags_addAndRemoveTags() {
        // 1) 기존에 DB에 저장된 RecipeTag 목록 (예: SOLO, PICNIC)
        RecipeTag existingSolo = RecipeTag.builder()
                .id(100L)
                .recipe(dummyRecipe)
                .tag(TagType.SOLO)
                .build();
        RecipeTag existingPicnic = RecipeTag.builder()
                .id(101L)
                .recipe(dummyRecipe)
                .tag(TagType.PICNIC)
                .build();
        List<RecipeTag> existingList = List.of(existingSolo, existingPicnic);

        // 2) 새롭게 들어온 태그 목록 (displayName): SOLO(유지), HEALTHY(추가)
        List<String> newTagDisplay = List.of("🍽️ 혼밥", "🥗 다이어트 / 건강식");

        // 기존 목록 반환
        when(recipeTagRepository.findByRecipeId(dummyRecipe.getId()))
                .thenReturn(existingList);

        // deleteAll은 void → 제거하려는 리스트만 캡처
        doNothing().when(recipeTagRepository).deleteAll(anyList());
        // save는 RecipeTag 반환
        when(recipeTagRepository.save(any(RecipeTag.class)))
                .thenAnswer(invocation -> invocation.getArgument(0));

        // 3) 실제 호출
        recipeTagService.updateTags(dummyRecipe, newTagDisplay);

        // 4-a) 기존 PICNIC 태그는 제거되어야 하므로 deleteAll(...)에 포함
        ArgumentCaptor<List<RecipeTag>> removeCaptor = ArgumentCaptor.forClass(List.class);
        verify(recipeTagRepository, times(1)).deleteAll(removeCaptor.capture());
        List<RecipeTag> toRemove = removeCaptor.getValue();
        assertEquals(1, toRemove.size());
        assertEquals(TagType.PICNIC, toRemove.get(0).getTag());

        // 4-b) 새로운 HEALTHY 태그는 save(...)로 한 번 저장되어야 함
        ArgumentCaptor<RecipeTag> addCaptor = ArgumentCaptor.forClass(RecipeTag.class);
        verify(recipeTagRepository, times(1)).save(addCaptor.capture());
        RecipeTag addedTag = addCaptor.getValue();
        assertEquals(dummyRecipe.getId(), addedTag.getRecipe().getId());
        assertEquals(TagType.HEALTHY, addedTag.getTag());

        // - 기존 SOLO 태그는 유지되어 delete나 save 대상이 아님
    }

    @Test
    @DisplayName("deleteAllByRecipeId: repository.deleteByRecipeId 메서드가 호출된다")
    void deleteAllByRecipeId_callsRepository() {
        // deleteByRecipeId는 void → doNothing() 정상 동작
        doNothing().when(recipeTagRepository).deleteByRecipeId(dummyRecipe.getId());

        recipeTagService.deleteAllByRecipeId(dummyRecipe.getId());

        verify(recipeTagRepository, times(1)).deleteByRecipeId(dummyRecipe.getId());
    }
}
