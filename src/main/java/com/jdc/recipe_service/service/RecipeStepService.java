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
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeStepMapper;
import com.jdc.recipe_service.mapper.StepIngredientMapper;
import com.jdc.recipe_service.util.ActionImageService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeStepService {
    private final IngredientRepository ingredientRepository;
    private final RecipeStepRepository recipeStepRepository;
    private final RecipeStepIngredientRepository recipeStepIngredientRepository;
    private final ActionImageService actionImageService;

    public List<RecipeStep> getStepsByRecipeId(Long recipeId) {
        return recipeStepRepository.findByRecipeIdOrderByStepNumber(recipeId);
    }

    public void saveAll(Recipe recipe, List<RecipeStepRequestDto> dtos) {
        int actionImageIndex = actionImageService.generateRandomIndex(); // ✅ 같은 레시피 내 고정

        for (RecipeStepRequestDto dto : dtos) {
            if (actionImageService.isSupportedAction(dto.getAction())) {
                String imageKey = actionImageService.generateImageKey(dto.getAction(), actionImageIndex);
                dto.setImageKey(imageKey); // ✅ S3 key 저장
            }

            RecipeStep step = RecipeStepMapper.toEntity(dto, recipe);
            recipeStepRepository.save(step);

            if (dto.getIngredients() == null) continue;

            for (RecipeStepIngredientRequestDto ingDto : dto.getIngredients()) {
                Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(ingDto.getName().trim())
                        .orElseThrow(() -> new CustomException(ErrorCode.INGREDIENT_NOT_FOUND, ingDto.getName()));

                RecipeStepIngredient rsi = StepIngredientMapper.toEntity(
                        ingDto, step, ingredient
                );
                recipeStepIngredientRepository.save(rsi);
            }
        }
    }


    public void saveAllFromUser(Recipe recipe, List<RecipeStepRequestDto> dtos) {
        for (RecipeStepRequestDto dto : dtos) {
            RecipeStep step = RecipeStepMapper.toEntity(dto, recipe);
            recipeStepRepository.save(step);

            if (dto.getIngredients() == null) continue;

            for (RecipeStepIngredientRequestDto ingDto : dto.getIngredients()) {
                Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(ingDto.getName().trim())
                        .orElseThrow(() -> new CustomException(ErrorCode.INGREDIENT_NOT_FOUND, ingDto.getName()));

                String quantityRaw = ingDto.getQuantity().trim();
                RecipeStepIngredient rsi = StepIngredientMapper.toEntity(
                        new RecipeStepIngredientRequestDto(ingDto.getName(), quantityRaw), step, ingredient
                );
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
                step.updateStepImageKey(dto.getImageKey());
                step.updateAction(dto.getAction());
            } else {
                step = RecipeStepMapper.toEntity(dto, recipe);
                step = recipeStepRepository.save(step);
            }

            updateStepIngredients(step, dto.getIngredients());
        }
    }

    public void updateStepsFromUser(Recipe recipe, List<RecipeStepRequestDto> stepDtos) {
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
                step.updateStepImageKey(dto.getImageKey());
            } else {
                step = RecipeStepMapper.toEntity(dto, recipe);
                step = recipeStepRepository.save(step);
            }

            updateStepIngredients(step, dto.getIngredients());
        }
    }


    private void updateStepIngredients(RecipeStep step, List<RecipeStepIngredientRequestDto> dtos) {
        if (dtos == null) return;

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
            Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(dto.getName().trim())
                    .orElseThrow(() -> new CustomException(ErrorCode.INGREDIENT_NOT_FOUND, dto.getName()));

            String rawQuantity = dto.getQuantity().trim(); //문자열 그대로 사용

            RecipeStepIngredient existingIng = existingMap.get(key);
            if (existingIng != null) {
                if (!Objects.equals(existingIng.getQuantity(), rawQuantity)) {
                    existingIng.updateQuantityAndUnit(rawQuantity, ingredient.getUnit());
                    recipeStepIngredientRepository.save(existingIng);
                }
            } else {
                RecipeStepIngredient newIng = StepIngredientMapper.toEntity(
                        new RecipeStepIngredientRequestDto(dto.getName(), rawQuantity), step, ingredient
                );
                recipeStepIngredientRepository.save(newIng);
                step.getStepIngredients().add(newIng);
            }
        }
    }

    @Transactional
    public void deleteAllByRecipeId(Long recipeId) {
        List<RecipeStep> steps = recipeStepRepository.findByRecipeIdOrderByStepNumber(recipeId);
        for (RecipeStep step : steps) {
            recipeStepIngredientRepository.deleteByStepId(step.getId());
        }
        recipeStepRepository.deleteByRecipeId(recipeId);
    }


    private double parseQuantity(String quantityStr) {
        quantityStr = quantityStr.trim();
        if (quantityStr.contains("/")) {
            String[] parts = quantityStr.split("/");
            if (parts.length == 2) {
                double numerator = Double.parseDouble(parts[0]);
                double denominator = Double.parseDouble(parts[1]);
                return numerator / denominator;
            } else {
                throw new NumberFormatException("Invalid fraction: " + quantityStr);
            }
        }
        return Double.parseDouble(quantityStr);
    }

    private String formatQuantityForDisplay(double value) {
        return value % 1 == 0 ? String.valueOf((int) value) : String.valueOf(value);
    }
}

