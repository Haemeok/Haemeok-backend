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
        int actionImageIndex = actionImageService.generateRandomIndex();

        for (RecipeStepRequestDto dto : dtos) {
            if (actionImageService.isSupportedAction(dto.getAction())) {
                dto.setImageKey(actionImageService.generateImageKey(dto.getAction(), actionImageIndex));
            }

            RecipeStep step = RecipeStepMapper.toEntity(dto, recipe);
            recipeStepRepository.save(step);
            saveStepIngredients(dto.getIngredients(), step, riMap);
        }
    }

    @Transactional
    public void saveAllFromUser(Recipe recipe, List<RecipeStepRequestDto> dtos) {
        Map<String, RecipeIngredient> riMap = loadRecipeIngredientMap(recipe.getId());

        for (RecipeStepRequestDto dto : dtos) {
            RecipeStep step = RecipeStepMapper.toEntity(dto, recipe);
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

        for (RecipeStepRequestDto dto : dtos) {
            RecipeStep step = existingMap.get(dto.getStepNumber());
            if (step == null) {
                step = RecipeStepMapper.toEntity(dto, recipe);
                recipeStepRepository.save(step);
            } else {
                step.updateInstruction(dto.getInstruction());
                step.updateStepImageKey(dto.getImageKey());
                step.updateAction(dto.getAction());
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
                recipeStepRepository.save(step);
            } else {
                step.updateInstruction(dto.getInstruction());
                step.updateStepImageKey(dto.getImageKey());
            }
            updateStepIngredients(step, dto.getIngredients(), riMap);
        }
    }

    private void saveStepIngredients(
            List<RecipeStepIngredientRequestDto> dtos,
            RecipeStep step,
            Map<String, RecipeIngredient> riMap
    ) {
        if (dtos == null) return;

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
                        i -> i.getIngredient() != null
                                ? "id:" + i.getIngredient().getId()
                                : "custom:" + i.getCustomName().toLowerCase().trim(),
                        Function.identity(),
                        (a, b) -> a
                ));

        Set<String> newKeys = dtos.stream()
                .map(d -> {
                    String name = d.getName() != null ? d.getName() : d.getCustomName();
                    if (name == null || name.isBlank()) {
                        throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "재료명(name/customName)은 필수입니다.");
                    }
                    RecipeIngredient ri = riMap.get(name.toLowerCase().trim());
                    if (ri == null) {
                        throw new CustomException(ErrorCode.INGREDIENT_NOT_FOUND, name);
                    }
                    return ri.getIngredient() != null
                            ? "id:" + ri.getIngredient().getId()
                            : "custom:" + ri.getCustomName().toLowerCase().trim();
                })
                .collect(Collectors.toSet());

        existing.stream()
                .filter(i -> {
                    String key = i.getIngredient() != null
                            ? "id:" + i.getIngredient().getId()
                            : "custom:" + i.getCustomName().toLowerCase().trim();
                    return !newKeys.contains(key);
                })
                .forEach(i -> {
                    step.getStepIngredients().remove(i);
                    recipeStepIngredientRepository.delete(i);
                });

        for (RecipeStepIngredientRequestDto dto : dtos) {
            String rawName = dto.getName() != null ? dto.getName() : dto.getCustomName();
            if (rawName == null || rawName.isBlank()) {
                throw new CustomException(ErrorCode.INVALID_INPUT_VALUE, "재료명(name/customName)은 필수입니다.");
            }

            String key = rawName.toLowerCase().trim();
            RecipeIngredient ri = riMap.get(key);
            if (ri == null) {
                throw new CustomException(ErrorCode.INGREDIENT_NOT_FOUND, rawName);
            }

            String matchKey = ri.getIngredient() != null
                    ? "id:" + ri.getIngredient().getId()
                    : "custom:" + ri.getCustomName().toLowerCase().trim();

            RecipeStepIngredient exist = existingMap.get(matchKey);
            if (exist != null) {
                String rawQty = dto.getQuantity() != null ? dto.getQuantity().trim() : ri.getQuantity(); // ✅ 상위 재료에서 대체
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


    @Transactional
    public void deleteAllByRecipeId(Long recipeId) {
        List<RecipeStep> steps = recipeStepRepository.findByRecipeIdOrderByStepNumber(recipeId);
        for (RecipeStep step : steps) {
            recipeStepIngredientRepository.deleteByStepId(step.getId());
        }
        recipeStepRepository.deleteByRecipeId(recipeId);
    }

    private Map<String, RecipeIngredient> loadRecipeIngredientMap(Long recipeId) {
        return recipeIngredientRepository
                .findByRecipeId(recipeId)
                .stream()
                .collect(Collectors.toMap(
                        ri -> {
                            if (ri.getIngredient() != null) {
                                return ri.getIngredient().getName().toLowerCase().trim();
                            } else {
                                return ri.getCustomName().toLowerCase().trim();
                            }
                        },
                        Function.identity()
                ));
    }
}
