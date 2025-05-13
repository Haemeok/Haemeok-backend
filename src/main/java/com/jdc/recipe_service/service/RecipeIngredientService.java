package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.exception.CustomException;
import com.jdc.recipe_service.exception.ErrorCode;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
public class RecipeIngredientService {
    private final IngredientRepository ingredientRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;

    public int saveAll(Recipe recipe, List<RecipeIngredientRequestDto> dtos) {
        int totalCost = 0;

        for (RecipeIngredientRequestDto dto : dtos) {
            // 1. 수량 파싱
            double quantity;
            try {
                quantity = parseQuantity(dto.getQuantity());
            } catch (NumberFormatException e) {
                throw new CustomException(ErrorCode.INVALID_INGREDIENT_QUANTITY, dto.getQuantity());
            }

            // 2. 재료 탐색 (존재 여부 확인)
            Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(dto.getName()).orElse(null);

            int unitPrice;
            if (ingredient != null) {
                unitPrice = (ingredient.getPrice() != null) ? ingredient.getPrice() : 0;
            } else {
                // 없는 재료면 가격, 단위 필수
                if (dto.getCustomPrice() == null || dto.getCustomUnit() == null) {
                    throw new CustomException(ErrorCode.CUSTOM_INGREDIENT_INFO_MISSING, dto.getName());
                }
                unitPrice = dto.getCustomPrice().intValue();
            }

            int calculatedPrice = (int) Math.round(quantity * unitPrice);
            totalCost += (int) Math.round(quantity * unitPrice);

            RecipeIngredient entity = RecipeIngredientMapper.toEntity(dto, recipe, ingredient, calculatedPrice);
            recipeIngredientRepository.save(entity);
        }

        return totalCost;
    }


    public void updateIngredients(Recipe recipe, List<RecipeIngredientRequestDto> ingredientDtos) {
        recipeIngredientRepository.deleteByRecipeId(recipe.getId());
        recipeIngredientRepository.flush(); // 중복 방지
        saveAll(recipe, ingredientDtos);
    }

    public int updateIngredientsFromUser(Recipe recipe, List<RecipeIngredientRequestDto> dtos) {
        recipeIngredientRepository.deleteByRecipeId(recipe.getId()); // 1. 기존 삭제
        recipeIngredientRepository.flush(); // 💡 즉시 반영해서 중복 방지
        return saveAll(recipe, dtos); // 2. 새로 저장
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

    // ✅ 보여줄 때: 1.0 → "1", 1/2 → "1/2" 그대로
    public static String formatQuantityForDisplay(String originalInput) {
        try {
            double value = parseQuantityStatic(originalInput);
            if (value == (int) value) {
                return String.valueOf((int) value);
            } else {
                return originalInput;
            }
        } catch (Exception e) {
            return originalInput; // 파싱 실패 시 그대로 출력
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

