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
import com.jdc.recipe_service.domain.type.RobotType;
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

    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepRepository recipeStepRepository;
    private final RecipeStepIngredientRepository recipeStepIngredientRepository;
    private final ActionImageService actionImageService;

    @Transactional(readOnly = true)
    public List<RecipeStep> getStepsByRecipeId(Long recipeId) {
        return recipeStepRepository.findByRecipeIdOrderByStepNumber(recipeId);
    }

    @Transactional
    public void saveAll(Recipe recipe, List<RecipeStepRequestDto> dtos) {
        Map<String, RecipeIngredient> riMap = loadRecipeIngredientMap(recipe.getId());

        for (RecipeStepRequestDto dto : dtos) {
            RecipeStep step = RecipeStepMapper.toEntity(dto, recipe);
            if (dto.getImageKey() != null) {
                step.updateStepImageKey(dto.getImageKey());
            }
            recipeStepRepository.save(step);
            saveStepIngredients(dto.getIngredients(), step, riMap);
        }
    }

    @Transactional
    public void updateSteps(Recipe recipe, List<RecipeStepRequestDto> dtos) {
        Map<String, RecipeIngredient> riMap = loadRecipeIngredientMap(recipe.getId());
        List<RecipeStep> existing = recipeStepRepository.findByRecipeIdOrderByStepNumber(recipe.getId());
        Map<Integer, RecipeStep> existingMap = existing.stream()
                .collect(Collectors.toMap(RecipeStep::getStepNumber, Function.identity()));
        Set<Integer> newNums = dtos.stream()
                .map(RecipeStepRequestDto::getStepNumber)
                .collect(Collectors.toSet());

        existing.stream()
                .filter(s -> !newNums.contains(s.getStepNumber()))
                .forEach(recipeStepRepository::delete);

        boolean ai = recipe.isAiGenerated();

        for (RecipeStepRequestDto dto : dtos) {
            RecipeStep step = existingMap.get(dto.getStepNumber());
            if (step == null) {
                step = RecipeStepMapper.toEntity(dto, recipe);
                recipeStepRepository.save(step);
            } else {
                step.updateInstruction(dto.getInstruction());
                step.updateAction(dto.getAction());
            }
            if (ai) {
                String key = actionImageService.generateImageKey(RobotType.CLASSIC, dto.getAction());
                step.updateStepImageKey(key);
            }else {
                step.updateStepImageKey(dto.getImageKey());
            }
            updateStepIngredients(step, dto.getIngredients(), riMap);
        }
    }

    @Transactional
    public void updateStepsFromUser(Recipe recipe, List<RecipeStepRequestDto> dtos) {
        Map<String, RecipeIngredient> riMap = loadRecipeIngredientMap(recipe.getId());
        List<RecipeStep> existing = recipeStepRepository.findByRecipeIdOrderByStepNumber(recipe.getId());
        Map<Integer, RecipeStep> existingMap = existing.stream()
                .collect(Collectors.toMap(RecipeStep::getStepNumber, Function.identity()));
        Set<Integer> newNums = dtos.stream()
                .map(RecipeStepRequestDto::getStepNumber)
                .collect(Collectors.toSet());

        existing.stream()
                .filter(s -> !newNums.contains(s.getStepNumber()))
                .forEach(recipeStepRepository::delete);

        for (RecipeStepRequestDto dto : dtos) {
            RecipeStep step = existingMap.get(dto.getStepNumber());
            if (step == null) {
                step = RecipeStepMapper.toEntity(dto, recipe);
            } else {
                step.updateInstruction(dto.getInstruction());
            }
            step.updateStepImageKey(dto.getImageKey());
            recipeStepRepository.save(step);
            updateStepIngredients(step, dto.getIngredients(), riMap);
        }
    }

    private void saveStepIngredients(
            List<RecipeStepIngredientRequestDto> dtos,
            RecipeStep step,
            Map<String, RecipeIngredient> riMap
    ) {
        if (dtos == null || dtos.isEmpty()) return;

        for (RecipeStepIngredientRequestDto dto : dtos) {
            String key = dto.getName().trim().toLowerCase();
            RecipeIngredient ri = riMap.get(key);
            if (ri == null) {
                throw new CustomException(ErrorCode.INGREDIENT_NOT_FOUND, dto.getName());
            }
            RecipeStepIngredient rsi = StepIngredientMapper.toEntity(dto, step, ri);
            recipeStepIngredientRepository.save(rsi);
        }
    }

    private void updateStepIngredients(
            RecipeStep step,
            List<RecipeStepIngredientRequestDto> dtos,
            Map<String, RecipeIngredient> riMap
    ) {
        if (dtos == null) return;

        List<RecipeStepIngredient> existing = new ArrayList<>(step.getStepIngredients());
        Map<String, RecipeStepIngredient> existingMap = existing.stream()
                .filter(i -> i.getIngredient() != null || i.getCustomName() != null)
                .collect(Collectors.toMap(
                        RecipeStepService::generateKey,
                        Function.identity(),
                        (a, b) -> a
                ));

        Set<String> newKeys = dtos.stream()
                .map(d -> {
                    String name = d.getName() != null ? d.getName() : d.getCustomName();
                    validateNamePresent(name);

                    RecipeIngredient ri = riMap.get(name.toLowerCase().trim());
                    if (ri == null) {
                        throw new CustomException(ErrorCode.INGREDIENT_NOT_FOUND, name);
                    }
                    return generateKey(ri);
                })
                .collect(Collectors.toSet());

        existing.stream()
                .filter(i -> !newKeys.contains(generateKey(i)))
                .forEach(i -> {
                    step.getStepIngredients().remove(i);
                    recipeStepIngredientRepository.delete(i);
                });

        for (RecipeStepIngredientRequestDto dto : dtos) {
            String rawName = dto.getName() != null ? dto.getName() : dto.getCustomName();
            validateNamePresent(rawName);

            String key = rawName.toLowerCase().trim();
            RecipeIngredient ri = riMap.get(key);
            if (ri == null) {
                throw new CustomException(ErrorCode.INGREDIENT_NOT_FOUND, rawName);
            }

            String matchKey = generateKey(ri);

            RecipeStepIngredient exist = existingMap.get(matchKey);
            if (exist != null) {
                String rawQty = dto.getQuantity() != null ? dto.getQuantity().trim() : ri.getQuantity();
                String newUnit = ri.getIngredient() == null
                        ? dto.getCustomUnit()
                        : ri.getIngredient().getUnit();

                if (!Objects.equals(exist.getQuantity(), rawQty)
                        || !Objects.equals(exist.getUnit(), newUnit)) {
                    exist.updateQuantityAndUnit(rawQty, newUnit);
                    recipeStepIngredientRepository.save(exist);
                }
            } else {
                RecipeStepIngredient newIng = StepIngredientMapper.toEntity(dto, step, ri);
                recipeStepIngredientRepository.save(newIng);
                step.getStepIngredients().add(newIng);
            }
        }
    }

    private void deleteAllStepIngredients(RecipeStep step) {
        recipeStepIngredientRepository.deleteByStepId(step.getId());
    }

    @Transactional
    public void deleteAllByRecipeId(Long recipeId) {
        List<RecipeStep> steps = recipeStepRepository.findByRecipeIdOrderByStepNumber(recipeId);
        for (RecipeStep step : steps) {
            deleteAllStepIngredients(step);
        }
        recipeStepRepository.deleteByRecipeId(recipeId);
    }

    private Map<String, RecipeIngredient> loadRecipeIngredientMap(Long recipeId) {
        return recipeIngredientRepository
                .findByRecipeId(recipeId)
                .stream()
                .collect(Collectors.toMap(
                        ri -> ri.getIngredient() != null
                                ? ri.getIngredient().getName().toLowerCase().trim()
                                : ri.getCustomName().toLowerCase().trim(),
                        Function.identity()
                ));
    }

    private void validateNamePresent(String name) {
        if (name == null || name.isBlank()) {
            throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "재료명(name/customName)은 필수입니다.");
        }
    }


    private static String generateKey(RecipeIngredient ri) {
        return ri.getIngredient() != null
                ? "id:" + ri.getIngredient().getId()
                : "custom:" + ri.getCustomName().toLowerCase().trim();
    }

    private static String generateKey(RecipeStepIngredient i) {
        return i.getIngredient() != null
                ? "id:" + i.getIngredient().getId()
                : "custom:" + i.getCustomName().toLowerCase().trim();
    }
}
