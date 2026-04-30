package com.jdc.recipe_service.dev.service.ingredient;

import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
import com.jdc.recipe_service.service.IngredientService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import java.math.BigDecimal;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

@ExtendWith(MockitoExtension.class)
class DevIngredientSearchServiceTest {

    @Mock
    private IngredientService ingredientService;

    @Mock
    private IngredientUnitRepository ingredientUnitRepository;

    @InjectMocks
    private DevIngredientSearchService service;

    @Test
    @DisplayName("search: replaces legacy unit with default ingredient_units label and falls back when no default exists")
    void search_usesDefaultUnitWithLegacyFallback() {
        Pageable pageable = PageRequest.of(1, 20);
        IngredientSummaryDto potato = new IngredientSummaryDto(
                10L, "potato", "vegetable", "potato.webp", "legacyUnit", null);
        IngredientSummaryDto garlic = new IngredientSummaryDto(
                20L, "garlic", "vegetable", "garlic.webp", "legacyGarlic", null);
        Page<IngredientSummaryDto> base = new PageImpl<>(List.of(potato, garlic), pageable, 44);

        Ingredient potatoEntity = Ingredient.builder().id(10L).name("potato").build();
        Ingredient garlicEntity = Ingredient.builder().id(20L).name("garlic").build();
        IngredientUnit potatoDefault = IngredientUnit.builder()
                .id(101L)
                .ingredient(potatoEntity)
                .unitLabelKo("piece")
                .gramsPerUnit(new BigDecimal("150.000"))
                .isDefault(true)
                .build();
        IngredientUnit garlicNonDefault = IngredientUnit.builder()
                .id(201L)
                .ingredient(garlicEntity)
                .unitLabelKo("spoon")
                .gramsPerUnit(new BigDecimal("15.000"))
                .isDefault(false)
                .build();

        given(ingredientService.search("potato", "vegetable", 7L, false, pageable)).willReturn(base);
        given(ingredientUnitRepository.findAllByIngredientIdIn(List.of(10L, 20L)))
                .willReturn(List.of(garlicNonDefault, potatoDefault));

        Page<IngredientSummaryDto> result = service.search("potato", "vegetable", 7L, pageable);

        assertThat(result.getContent()).hasSize(2);
        assertThat(result.getContent().get(0).getUnit()).isEqualTo("piece");
        assertThat(result.getContent().get(1).getUnit()).isEqualTo("legacyGarlic");
        assertThat(result.getPageable()).isEqualTo(pageable);
        assertThat(result.getTotalElements()).isEqualTo(44);
        verify(ingredientUnitRepository).findAllByIngredientIdIn(List.of(10L, 20L));
    }

    @Test
    @DisplayName("search: empty page skips ingredient_units lookup")
    void search_emptyPage_skipsUnitLookup() {
        Pageable pageable = PageRequest.of(0, 20);
        Page<IngredientSummaryDto> empty = Page.empty(pageable);
        given(ingredientService.search(null, null, null, false, pageable)).willReturn(empty);

        Page<IngredientSummaryDto> result = service.search(null, null, null, pageable);

        assertThat(result).isSameAs(empty);
        verifyNoInteractions(ingredientUnitRepository);
    }
}
