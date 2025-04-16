package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepIngredientRequestDto;
import com.jdc.recipe_service.domain.dto.recipe.step.RecipeStepRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeStep;
import com.jdc.recipe_service.domain.entity.RecipeStepIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepRepository;
import com.jdc.recipe_service.mapper.RecipeStepMapper;
import com.jdc.recipe_service.mapper.StepIngredientMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeStepService {
    private final IngredientRepository ingredientRepository;
    private final RecipeStepRepository recipeStepRepository;
    private final RecipeStepIngredientRepository recipeStepIngredientRepository;

    public void saveAll(Recipe recipe, List<RecipeStepRequestDto> dtos) {
        for (RecipeStepRequestDto dto : dtos) {
            RecipeStep step = RecipeStepMapper.toEntity(dto, recipe);
            recipeStepRepository.save(step);

            for (RecipeStepIngredientRequestDto ingDto : dto.getIngredients()) {
                Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(ingDto.getName().trim())
                        .orElseThrow(() -> new RuntimeException("재료가 존재하지 않습니다: " + ingDto.getName()));

                RecipeStepIngredient rsi = StepIngredientMapper.toEntity(ingDto, step, ingredient); // ✅ unit 자동 포함됨
                recipeStepIngredientRepository.save(rsi);
            }
        }
    }

    public void updateSteps(Recipe recipe, List<RecipeStepRequestDto> stepDtos) {
        List<RecipeStep> existingSteps = recipeStepRepository.findByRecipeIdOrderByStepNumber(recipe.getId());
        Map<Integer, RecipeStep> existingMap = existingSteps.stream()
                .collect(Collectors.toMap(RecipeStep::getStepNumber, Function.identity()));

        Set<Integer> newNumbers = stepDtos.stream().map(RecipeStepRequestDto::getStepNumber).collect(Collectors.toSet());

        for (RecipeStep step : existingSteps) {
            if (!newNumbers.contains(step.getStepNumber())) {
                recipeStepIngredientRepository.deleteByStepId(step.getId());
                recipeStepRepository.delete(step);
            }
        }

        for (RecipeStepRequestDto dto : stepDtos) {
            RecipeStep step = existingMap.get(dto.getStepNumber());
            if (step != null) {
                step.updateInstruction(dto.getInstruction());
                step.updateStepImageUrl(dto.getStepImageUrl());
//                step.updateCookingTools(dto.getCookingTools());
                step.updateAction(dto.getAction());
            } else {
                step = RecipeStepMapper.toEntity(dto, recipe);
                step = recipeStepRepository.save(step);
            }

            updateStepIngredients(step, dto.getIngredients());
        }
    }

    private void updateStepIngredients(RecipeStep step, List<RecipeStepIngredientRequestDto> dtos) {
        List<RecipeStepIngredient> existing = new ArrayList<>(step.getStepIngredients());
        Map<String, RecipeStepIngredient> existingMap = existing.stream()
                .collect(Collectors.toMap(i -> i.getIngredient().getName().toLowerCase().trim(), Function.identity()));

        Set<String> newNames = dtos.stream().map(i -> i.getName().toLowerCase().trim()).collect(Collectors.toSet());

        for (RecipeStepIngredient ing : existing) {
            if (!newNames.contains(ing.getIngredient().getName().toLowerCase().trim())) {
                step.getStepIngredients().remove(ing);
                recipeStepIngredientRepository.delete(ing);
            }
        }

        for (RecipeStepIngredientRequestDto dto : dtos) {
            String key = dto.getName().toLowerCase().trim();
            RecipeStepIngredient existingIng = existingMap.get(key);
            Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(dto.getName().trim())
                    .orElseThrow(() -> new RuntimeException("재료가 존재하지 않습니다: " + dto.getName()));

            if (existingIng != null) {
                if (!Objects.equals(existingIng.getQuantity(), dto.getQuantity())) {
                    existingIng.updateQuantityAndUnit(dto.getQuantity(), ingredient.getUnit());
                    recipeStepIngredientRepository.save(existingIng);
                }
            } else {
                RecipeStepIngredient newIng = StepIngredientMapper.toEntity(dto, step, ingredient);
                recipeStepIngredientRepository.save(newIng);
                step.getStepIngredients().add(newIng);
            }
        }
    }



}