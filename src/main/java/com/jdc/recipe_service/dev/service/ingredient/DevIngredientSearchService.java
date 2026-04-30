package com.jdc.recipe_service.dev.service.ingredient;

import com.jdc.recipe_service.domain.dto.ingredient.IngredientSummaryDto;
import com.jdc.recipe_service.domain.entity.IngredientUnit;
import com.jdc.recipe_service.domain.repository.IngredientUnitRepository;
import com.jdc.recipe_service.service.IngredientService;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Dev V3 ingredient search mirror.
 *
 * <p>Operational ingredient search returns legacy {@code ingredients.unit}. Dev V3 keeps the same
 * response shape but replaces {@code unit} with the default row from {@code ingredient_units}
 * when one exists, falling back to the legacy value for rows without a seeded default unit.
 */
@Service
@RequiredArgsConstructor
public class DevIngredientSearchService {

    private final IngredientService ingredientService;
    private final IngredientUnitRepository ingredientUnitRepository;

    @Transactional(readOnly = true)
    public Page<IngredientSummaryDto> search(String keyword,
                                             String category,
                                             Long userId,
                                             Pageable pageable) {
        Page<IngredientSummaryDto> base = ingredientService.search(keyword, category, userId, false, pageable);
        if (base.isEmpty()) {
            return base;
        }

        List<Long> ingredientIds = base.getContent().stream()
                .map(IngredientSummaryDto::getId)
                .filter(Objects::nonNull)
                .distinct()
                .toList();
        if (ingredientIds.isEmpty()) {
            return base;
        }

        Map<Long, String> defaultUnits = ingredientUnitRepository.findAllByIngredientIdIn(ingredientIds).stream()
                .filter(unit -> Boolean.TRUE.equals(unit.getIsDefault()))
                .filter(unit -> unit.getIngredient() != null && unit.getIngredient().getId() != null)
                .filter(unit -> unit.getUnitLabelKo() != null && !unit.getUnitLabelKo().isBlank())
                .sorted(Comparator.comparing(IngredientUnit::getId, Comparator.nullsLast(Long::compareTo)))
                .collect(Collectors.toMap(
                        unit -> unit.getIngredient().getId(),
                        IngredientUnit::getUnitLabelKo,
                        (first, ignored) -> first
                ));

        List<IngredientSummaryDto> content = base.getContent().stream()
                .map(dto -> new IngredientSummaryDto(
                        dto.getId(),
                        dto.getName(),
                        dto.getCategory(),
                        dto.getImageUrl(),
                        defaultUnits.getOrDefault(dto.getId(), dto.getUnit()),
                        dto.getInFridge()
                ))
                .toList();

        return new PageImpl<>(content, base.getPageable(), base.getTotalElements());
    }
}
