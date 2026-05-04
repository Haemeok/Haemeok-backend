package com.jdc.recipe_service.service.chat;

import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.repository.RecipeRepository;
import com.jdc.recipe_service.domain.type.DishType;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;

@ExtendWith(MockitoExtension.class)
class ChatRecipeLoaderTest {

    @Mock
    private RecipeRepository recipeRepository;

    @InjectMocks
    private ChatRecipeLoader loader;

    @Test
    @DisplayName("LLM 프롬프트 조리 단계는 stepNumber 오름차순과 실제 단계 번호를 사용한다")
    void loadAsPromptText_ordersStepsByStepNumber() {
        // given
        Recipe recipe = Recipe.builder()
                .title("순서 테스트")
                .dishType(DishType.RICE_NOODLE)
                .isPrivate(false)
                .steps(List.of(
                        RecipeStep.builder().stepNumber(2).instruction("둘째 단계").build(),
                        RecipeStep.builder().stepNumber(1).instruction("첫째 단계").build(),
                        RecipeStep.builder().stepNumber(3).instruction("셋째 단계").build()
                ))
                .build();
        given(recipeRepository.findById(60L)).willReturn(Optional.of(recipe));

        // when
        String promptText = loader.loadAsPromptText(1L, 60L);

        // then
        assertThat(promptText).contains("""
                조리법:
                1. 첫째 단계
                2. 둘째 단계
                3. 셋째 단계
                """);
    }
}
