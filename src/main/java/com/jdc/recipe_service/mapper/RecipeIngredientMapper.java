package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;

import java.util.List;

public class RecipeIngredientMapper {

    public static RecipeIngredient toEntity(RecipeIngredientRequestDto dto, Recipe recipe, Ingredient ingredient) {
        return RecipeIngredient.builder()
                .recipe(recipe)
                .ingredient(ingredient)
                .quantity(dto.getQuantity())
                .unit(ingredient.getUnit())
                .build();
    }

    public static RecipeIngredientDto toDto(RecipeIngredient entity) {
        int unitPrice = entity.getIngredient().getPrice() != null ? entity.getIngredient().getPrice() : 0;
        double quantityValue;

        try {
            quantityValue = parseQuantity(entity.getQuantity());
        } catch (Exception e) {
            quantityValue = 0.0; // 실패 시 가격은 0 처리
        }

        int totalPrice = (int) Math.round(quantityValue * unitPrice);

        return RecipeIngredientDto.builder()
                .ingredientId(entity.getIngredient().getId())
                .name(entity.getIngredient().getName())
                .quantity(formatQuantityForDisplay(entity.getQuantity())) // ✅ 포맷팅 적용
                .unit(entity.getUnit())
                .price(totalPrice)
                .build();
    }

    public static List<RecipeIngredientDto> toDtoList(List<RecipeIngredient> entities) {
        return entities.stream().map(RecipeIngredientMapper::toDto).toList();
    }

    // ✅ 숫자 변환 로직 (fraction or decimal)
    private static double parseQuantity(String quantityStr) {
        quantityStr = quantityStr.trim();
        if (quantityStr.contains("/")) {
            String[] parts = quantityStr.split("/");
            if (parts.length == 2) {
                double numerator = Double.parseDouble(parts[0]);
                double denominator = Double.parseDouble(parts[1]);
                return numerator / denominator;
            } else {
                throw new NumberFormatException("잘못된 분수: " + quantityStr);
            }
        }
        return Double.parseDouble(quantityStr);
    }

    // ✅ 포맷팅 로직 (1.0 → 1)
    private static String formatQuantityForDisplay(String quantityStr) {
        try {
            double value = parseQuantity(quantityStr);
            if (value == (int) value) {
                return Integer.toString((int) value);
            } else {
                return quantityStr; // 분수나 소수 그대로
            }
        } catch (Exception e) {
            return quantityStr; // 파싱 실패 시 원본 그대로
        }
    }
}
