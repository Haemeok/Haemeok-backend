package com.jdc.recipe_service.mapper;

import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientDto;
import com.jdc.recipe_service.domain.dto.recipe.ingredient.RecipeIngredientRequestDto;
import com.jdc.recipe_service.domain.entity.Ingredient;
import com.jdc.recipe_service.domain.entity.Recipe;
import com.jdc.recipe_service.domain.entity.RecipeIngredient;
import com.jdc.recipe_service.domain.type.RecipeSourceType;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

public class RecipeIngredientMapper {

    public static RecipeIngredient toEntity(
            RecipeIngredientRequestDto dto,
            Recipe recipe,
            Ingredient masterIngredient,
            int calculatedPriceForItem,
            String unitForRecipeItemField, // saveAll에서 결정된, 이 레시피 항목에 사용될 최종 단위
            RecipeSourceType sourceType) {

        RecipeIngredient.RecipeIngredientBuilder builder = RecipeIngredient.builder()
                .recipe(recipe)
                .quantity(dto.getQuantity())
                .price(calculatedPriceForItem)
                .unit(unitForRecipeItemField);

        if (masterIngredient != null) {
            builder.ingredient(masterIngredient);
            builder.customName(null);
            builder.customPrice(null);
            builder.customUnit(null);
        } else {
            builder.ingredient(null);
            builder.customName(dto.getName());
            builder.customUnit(unitForRecipeItemField); // 새로운 재료의 "정의 단위"는 saveAll에서 결정된 unitForRecipeItemField 사용

            if (sourceType == RecipeSourceType.AI) {
                builder.customPrice(BigDecimal.ZERO); // AI가 생성한 새로운 재료의 "단가"는 0
            } else { // 사용자가 직접 입력한 새로운 재료
                // dto.getCustomPrice()는 사용자가 입력한 단가
                builder.customPrice(dto.getCustomPrice() != null ? dto.getCustomPrice() : BigDecimal.ZERO);
            }
        }
        return builder.build();
    }

    public static RecipeIngredientDto toDto(RecipeIngredient entity) {
        Ingredient ingredient = entity.getIngredient();
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

    private static String formatQuantityForDisplay(String quantityStr) {
        try {
            double value = parseQuantity(quantityStr);
            if (value == (int) value) {
                return Integer.toString((int) value);
            } else {
                return quantityStr;
            }
        } catch (Exception e) {
            return quantityStr;
        }
    }
}
