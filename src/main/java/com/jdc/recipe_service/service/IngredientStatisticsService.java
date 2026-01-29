package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Slf4j
public class IngredientStatisticsService {

    private final IngredientRepository ingredientRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;

    @Transactional
    public int updateAllIngredientPopularity() {
        long startTime = System.currentTimeMillis();

        List<Object[]> usageResults = recipeIngredientRepository.countIngredientsUsage();
        Map<Long, Long> usageMap = usageResults.stream()
                .collect(Collectors.toMap(
                        result -> (Long) result[0],
                        result -> (Long) result[1]
                ));

        List<Ingredient> allIngredients = ingredientRepository.findAll();

        int updatedCount = 0;
        for (Ingredient ingredient : allIngredients) {
            Long count = usageMap.getOrDefault(ingredient.getId(), 0L);

            if (!count.equals(ingredient.getUsageCount())) {
                ingredient.updateUsageCount(count);
                updatedCount++;
            }
        }

        long duration = System.currentTimeMillis() - startTime;
        log.info(">>>> [Batch Logic] Ingredient Popularity Updated. (Total: {}, Updated: {}, Time: {}ms)",
                allIngredients.size(), updatedCount, duration);

        return updatedCount;
    }
}
