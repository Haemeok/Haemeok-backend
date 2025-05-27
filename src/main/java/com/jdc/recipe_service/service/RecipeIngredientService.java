package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeStepIngredientRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RecipeIngredientService {
    private final IngredientRepository ingredientRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;
    private final RecipeStepIngredientRepository recipeStepIngredientRepository;

    public int saveAll(Recipe recipe, List<RecipeIngredientRequestDto> dtos) {
        int totalCost = 0;

        // ✅ 1. 전체 재료 캐싱
        Map<String, Ingredient> ingredientMap = ingredientRepository.findAll().stream()
                .collect(Collectors.toMap(i -> i.getName().toLowerCase().trim(), Function.identity()));

        for (RecipeIngredientRequestDto dto : dtos) {
            String nameKey = dto.getName().toLowerCase().trim();

            // 2. 수량 파싱
            double quantity;
            try {
                quantity = parseQuantity(dto.getQuantity());
            } catch (NumberFormatException e) {
                throw new CustomException(ErrorCode.INVALID_INGREDIENT_QUANTITY, dto.getQuantity());
            }

            // 3. 재료 탐색 (Map에서 조회)
            Ingredient ingredient = ingredientMap.get(nameKey);

            int unitPrice;
            if (ingredient != null) {
                unitPrice = (ingredient.getPrice() != null) ? ingredient.getPrice() : 0;
            } else {
                if (dto.getCustomPrice() == null || dto.getCustomUnit() == null) {
                    throw new CustomException(ErrorCode.CUSTOM_INGREDIENT_INFO_MISSING, dto.getName());
                }
                unitPrice = dto.getCustomPrice().intValue();
            }

            int calculatedPrice = (int) Math.round(quantity * unitPrice);
            totalCost += calculatedPrice;

            RecipeIngredient entity = RecipeIngredientMapper.toEntity(dto, recipe, ingredient, calculatedPrice);
            recipeIngredientRepository.save(entity);
        }

        return totalCost;
    }

    public int updateIngredients(Recipe recipe, List<RecipeIngredientRequestDto> ingredientDtos) {
        recipeIngredientRepository.deleteByRecipeId(recipe.getId());
        recipeIngredientRepository.flush();
        return saveAll(recipe, ingredientDtos);
    }

    @Transactional
    public int updateIngredientsFromUser(Recipe recipe, List<RecipeIngredientRequestDto> dtos) {
        List<RecipeIngredient> existingIngredients = recipeIngredientRepository.findByRecipeId(recipe.getId());
        for (RecipeIngredient ri : existingIngredients) {
            recipeStepIngredientRepository.deleteByRecipeIngredientId(ri.getId());
        }
        recipeIngredientRepository.flush();
        recipeIngredientRepository.deleteByRecipeId(recipe.getId());
        recipeIngredientRepository.flush();
        return saveAll(recipe, dtos);
    }


    @Transactional
    public void deleteAllByRecipeId(Long recipeId) {
        recipeIngredientRepository.deleteByRecipeId(recipeId);
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
                throw new NumberFormatException("잘못된 분수 형식입니다: " + quantityStr);
            }
        }
        return Double.parseDouble(quantityStr);
    }

    public static String formatQuantityForDisplay(String originalInput) {
        try {
            double value = parseQuantityStatic(originalInput);
            if (value == (int) value) {
                return String.valueOf((int) value);
            } else {
                return originalInput;
            }
        } catch (Exception e) {
            return originalInput;
        }
    }

    private static double parseQuantityStatic(String quantityStr) {
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
}
