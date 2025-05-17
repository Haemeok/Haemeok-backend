package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;

import java.util.List;
import java.util.Optional;

public class RecipeIngredientMapper {

    public static RecipeIngredient toEntity(RecipeIngredientRequestDto dto, Recipe recipe, Ingredient ingredient, int calculatedPrice) {
        return RecipeIngredient.builder()
                .recipe(recipe)
                .ingredient(ingredient)
                .customName(ingredient == null ? dto.getName() : null)
                .customPrice(ingredient == null ? dto.getCustomPrice() : null)
                .customUnit(ingredient == null ? dto.getCustomUnit() : null)
                .quantity(dto.getQuantity())
                .unit(ingredient != null ? ingredient.getUnit() : null)
                .price(calculatedPrice)
                .build();
    }

    public static RecipeIngredientDto toDto(RecipeIngredient entity) {
        Ingredient ingredient = entity.getIngredient(); // ⭐ null 가능성 처리
        boolean isCustom = (ingredient == null);

        int totalPrice = Optional.ofNullable(entity.getPrice())
                .orElse(0);

        return RecipeIngredientDto.builder()
                .ingredientId(isCustom ? null : ingredient.getId())
                .name(isCustom ? entity.getCustomName() : ingredient.getName())
                .quantity(formatQuantityForDisplay(entity.getQuantity()))
                .unit(isCustom ? entity.getCustomUnit() : entity.getUnit())
                .price(totalPrice)
                .build();
    }


    public static List<RecipeIngredientDto> toDtoList(List<RecipeIngredient> entities) {
        return entities.stream().map(RecipeIngredientMapper::toDto).toList();
    }

    // ✅ 숫자 변환 로직 (fraction or decimal)
    private static double parseQuantity(String quantityStr) {
        quantityStr = quantityStr.trim();
        quantityStr = quantityStr.replaceAll("[^0-9./]", "");
        if (quantityStr.contains("/")) {
            String[] parts = quantityStr.split("/");
            if (parts.length == 2) {
                double numerator = Double.parseDouble(parts[0]);
                double denominator = Double.parseDouble(parts[1]);
                if (denominator == 0) throw new NumberFormatException("0으로 나눌 수 없습니다");
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
