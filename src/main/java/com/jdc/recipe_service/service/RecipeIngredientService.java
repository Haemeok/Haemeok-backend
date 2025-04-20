package com.jdc.recipe_service.service;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.repository.IngredientRepository;
import com.jdc.recipe_service.domain.repository.RecipeIngredientRepository;
import com.jdc.recipe_service.mapper.RecipeIngredientMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class RecipeIngredientService {
    private final IngredientRepository ingredientRepository;
    private final RecipeIngredientRepository recipeIngredientRepository;

    public int saveAll(Recipe recipe, List<RecipeIngredientRequestDto> dtos) {
        int totalCost = 0;

        for (RecipeIngredientRequestDto dto : dtos) {
            Ingredient ingredient = ingredientRepository.findByNameIgnoreCase(dto.getName())
                    .orElseThrow(() -> new RuntimeException("재료가 존재하지 않습니다: " + dto.getName()));

            // ✅ 문자열 수량 → double 변환
            double quantity;
            try {
                quantity = parseQuantity(dto.getQuantity());
            } catch (NumberFormatException e) {
                throw new RuntimeException("수량 형식이 올바르지 않습니다: " + dto.getQuantity());
            }

            int unitPrice = ingredient.getPrice() != null ? ingredient.getPrice() : 0;
            totalCost += (int) Math.round(quantity * unitPrice);

            RecipeIngredient entity = RecipeIngredientMapper.toEntity(dto, recipe, ingredient);
            recipeIngredientRepository.save(entity);
        }

        return totalCost;
    }

    public void updateIngredients(Recipe recipe, List<RecipeIngredientRequestDto> ingredientDtos) {
        recipeIngredientRepository.deleteByRecipeId(recipe.getId());
        recipeIngredientRepository.flush(); // 중복 방지
        saveAll(recipe, ingredientDtos);
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

